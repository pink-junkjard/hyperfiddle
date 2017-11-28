(ns hypercrud.browser.find-element
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [clojure.set :as set]
            [datascript.parser :as parser]
            [hypercrud.util.core :as util]))


(defrecord FindElement [name fields source-symbol splat? type])

(defrecord Field [id attribute doc cell-data->value])

(defn variable->fe [element]
  (let [name (str (:symbol element))]
    (map->FindElement
      {:name name
       :fields [(map->Field {:id (hash name)
                             :attribute nil
                             :doc nil
                             :cell-data->value identity})]
       :source-symbol nil
       :splat? false
       :type :variable})))

(defn pull-cell->fe [docs-lookup cell source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [unsplatted-pattern (if-not splat?
                                          pull-pattern
                                          (let [splat-attrs (-> (set (keys cell))
                                                                (set/difference (set pull-pattern))
                                                                vec)]
                                            (->> pull-pattern
                                                 (mapcat (fn [sym]
                                                           (if (= sym '*)
                                                             splat-attrs
                                                             [sym]))))))]
                 (->> unsplatted-pattern
                      (remove #(= :db/id %))
                      (mapv (fn [attr]
                              (map->Field {:id (hash [fe-name attr])
                                           :attribute attr
                                           :doc (get docs-lookup attr)
                                           :cell-data->value attr})))))
       :source-symbol source-symbol
       :splat? splat?
       :type :pull})))

(defn pull-many-cells->fe [docs-lookup column-cells source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [unsplatted-pattern (if-not splat?
                                          pull-pattern
                                          (let [splat-attrs (-> (reduce (fn [acc cell]
                                                                          (-> (keys cell)
                                                                              (set)
                                                                              (into acc)))
                                                                        #{}
                                                                        column-cells)
                                                                (set/difference (set pull-pattern))
                                                                sort
                                                                vec)]
                                            (->> pull-pattern
                                                 (mapcat (fn [sym]
                                                           (if (= sym '*)
                                                             splat-attrs
                                                             [sym]))))))]
                 (->> unsplatted-pattern
                      (remove #(= :db/id %))
                      (mapv (fn [attr]
                              (map->Field {:id (hash [fe-name attr])
                                           :attribute attr
                                           :doc (get docs-lookup attr)
                                           :cell-data->value attr})))))
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
                             :doc nil
                             :cell-data->value identity})]
       :source-symbol nil
       :splat? false
       :type :aggregate})))

(defn auto-fe-one-cell [docs-lookup element cell]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe element)

    datascript.parser.Pull
    (pull-cell->fe docs-lookup cell
                   (get-in element [:source :symbol])
                   (get-in element [:variable :symbol])
                   (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe element)))

(defn auto-fe-many-cells [docs-lookup element column-cells]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe element)

    datascript.parser.Pull
    (pull-many-cells->fe docs-lookup column-cells
                         (get-in element [:source :symbol])
                         (get-in element [:variable :symbol])
                         (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe element)))

(defn build-docs-lookup [ctx]
  (reduce (fn [acc fe]
            (reduce (fn [acc field]
                      (assoc acc (:field/attribute field) (:db/doc field)))
                    acc
                    (get-in fe [:find-element/form :form/field])))
          {}
          (get-in ctx [:fiddle :link-query/find-element])))

(defn auto-find-elements [result ctx]
  (case (get-in ctx [:fiddle :request/type])
    :entity (mlet [source-symbol (try-either (.-dbname (get-in ctx [:route :request-params :entity])))
                   :let [fe-name "entity"
                         pull-pattern (get-in ctx [:request :pull-exp])
                         docs-lookup (build-docs-lookup ctx)]]
              (cats/return
                (if-let [a (get-in ctx [:request :a])]
                  (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                    :db.cardinality/one
                    [(pull-cell->fe docs-lookup result source-symbol fe-name pull-pattern)]

                    :db.cardinality/many
                    [(pull-many-cells->fe docs-lookup result source-symbol fe-name pull-pattern)])
                  [(pull-cell->fe docs-lookup result source-symbol fe-name pull-pattern)])))

    :query (mlet [{:keys [qfind]} (try-either (parser/parse-query (get-in ctx [:request :query])))
                  :let [docs-lookup (build-docs-lookup ctx)]]
             (cats/return
               (condp = (type qfind)
                 datascript.parser.FindRel (mapv (partial auto-fe-many-cells docs-lookup) (:elements qfind) (util/transpose result))
                 datascript.parser.FindColl [(auto-fe-many-cells docs-lookup (:element qfind) result)]
                 datascript.parser.FindTuple (mapv (partial auto-fe-one-cell docs-lookup) (:elements qfind) result)
                 datascript.parser.FindScalar [(auto-fe-one-cell docs-lookup (:element qfind) result)])))

    :blank (either/right [])

    (either/right [])))
