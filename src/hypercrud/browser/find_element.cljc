(ns hypercrud.browser.find-element
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [contrib.data :refer [transpose]]
            [contrib.reactive :as r]
            [contrib.try :refer [try-either]]
            [datascript.parser :as parser]))


(defrecord FindElement [name fields source-symbol splat? type])

(defrecord Field [id attribute cell-data->value])

(defn variable->fe [element]
  (let [name (str (:symbol element))]
    (map->FindElement
      {:name name
       :fields [(map->Field {:id (hash name)
                             :attribute nil
                             :cell-data->value identity})]
       :source-symbol nil
       :splat? false
       :type :variable})))

(defn pull->fields [pull-pattern splat-attrs fe-name]
  (let [top-level-attrs (reduce (fn [acc sym]
                                  (cond
                                    (= sym '*) (into acc splat-attrs)
                                    :else (conj acc sym)))
                                []
                                pull-pattern)]
    (->> top-level-attrs
         (remove #(= :db/id %))
         (mapv (fn [attr]
                 (map->Field {:id (hash [fe-name attr])
                              :attribute attr
                              :cell-data->value attr}))))))

(defn pull-cell->fe [cell source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [splat-attrs (when splat?
                                      (-> (set (keys cell))
                                          (set/difference (set pull-pattern))
                                          vec))]
                    (pull->fields pull-pattern splat-attrs fe-name))
       :source-symbol source-symbol
       :splat? splat?
       :type :pull})))

(defn pull-many-cells->fe [column-cells source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [splat-attrs (when splat?
                                     (-> (reduce (fn [acc cell]
                                                   (-> (keys cell)
                                                       (set)
                                                       (into acc)))
                                                 #{}
                                                 column-cells)
                                         (set/difference (set pull-pattern))
                                         sort
                                         vec))]
                   (pull->fields pull-pattern splat-attrs fe-name))
       :source-symbol source-symbol
       :splat? splat?
       :type :pull})))

(defn aggregate->fe [element]
  (let [name (str (cons (get-in element [:fn :symbol])
                        (map #(second (first %))
                             (:args element))))]
    (map->FindElement
      {:name name
       :fields [(map->Field {:id (hash name)
                             :attribute nil
                             :cell-data->value identity})]
       :source-symbol nil
       :splat? false
       :type :aggregate})))

(defn auto-fe-one-cell [element cell]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe element)

    datascript.parser.Pull
    (pull-cell->fe cell
                   (get-in element [:source :symbol])
                   (get-in element [:variable :symbol])
                   (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe element)))

(defn auto-fe-many-cells [element column-cells]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe element)

    datascript.parser.Pull
    (pull-many-cells->fe column-cells
                         (get-in element [:source :symbol])
                         (get-in element [:variable :symbol])
                         (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe element)))

(defn auto-find-elements [{:keys [:hypercrud.browser/result :hypercrud.browser/request] :as ctx}]
  (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
    :entity (mlet [:let [[_ [e]] (:route ctx)]
                   source-symbol (try-either (.-dbname e))
                   :let [fe-name "entity"
                         pull-pattern @(r/cursor request [:pull-exp])]]
              (cats/return
                (if-let [a @(r/cursor request [:a])]
                  (case @(r/cursor (:hypercrud.browser/schemas ctx) [(str source-symbol) a :db/cardinality :db/ident])
                    :db.cardinality/one
                    [(pull-cell->fe @result source-symbol fe-name pull-pattern)]

                    :db.cardinality/many
                    [(pull-many-cells->fe @result source-symbol fe-name pull-pattern)])
                  [(pull-cell->fe @result source-symbol fe-name pull-pattern)])))

    :query (mlet [{:keys [qfind]} (try-either (parser/parse-query @(r/cursor request [:query])))]
             (cats/return
               (condp = (type qfind)
                 datascript.parser.FindRel (let [results-by-column (transpose @result)]
                                             (->> (:elements qfind)
                                                  (map-indexed (fn [idx element]
                                                                 (auto-fe-many-cells element (get results-by-column idx))))
                                                  vec))
                 datascript.parser.FindColl [(auto-fe-many-cells (:element qfind) @result)]
                 datascript.parser.FindTuple (mapv auto-fe-one-cell (:elements qfind) @result)
                 datascript.parser.FindScalar [(auto-fe-one-cell (:element qfind) @result)])))

    :blank (either/right [])

    (either/right [])))
