(ns hypercrud.browser.find-element
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [contrib.data :refer [transpose]]
            [contrib.reactive :as r]
            [contrib.try :refer [try-either]]
            [datascript.parser :as parser]))


(defrecord FindElement [name fields source-symbol splat? type])

(defrecord Field [id label attribute cell-data->value])

(def keyword->label #(some-> % name str))

(defn variable->fe [element]
  (let [name (str (:symbol element))]
    (map->FindElement
      {:name name
       :fields [(map->Field {:id (hash name)
                             :label name
                             :attribute nil
                             :cell-data->value identity})]
       :source-symbol nil
       :splat? false
       :type :variable})))

(let [alias->value (fn [alias cell-data] (get cell-data alias))]
  (defn attr-with-opts-or-expr [fe-name list-or-vec]
    (if (#{'default 'limit} (first list-or-vec))
      (let [attr (second list-or-vec)]
        (map->Field {:id (hash [fe-name attr])
                     :label (keyword->label attr)
                     :attribute attr
                     :cell-data->value attr}))
      ; otherwise attr-with-opts
      (let [[attr & {:as opts}] list-or-vec]
        (map->Field {:id (hash [fe-name attr])
                     :-alias (:as opts)
                     :label (if-let [alias (:as opts)]
                              (if (string? alias) alias (pr-str alias))
                              (keyword->label attr))
                     :attribute attr
                     :cell-data->value (or (some->> (:as opts) (r/partial alias->value)) attr)})))))

(defn pull->fields [pull-pattern inferred-attrs fe-name]
  (let [explicit-fields (reduce (fn [acc sym]
                                  (cond
                                    (map? sym) (->> (keys sym)
                                                    (map (fn [k]
                                                           (if (or (vector? k) (seq? k))
                                                             (attr-with-opts-or-expr fe-name k)
                                                             (map->Field {:id (hash [fe-name k])
                                                                          :label (keyword->label k)
                                                                          :attribute k
                                                                          :cell-data->value k}))))
                                                    (into acc))
                                    (or (vector? sym) (seq? sym)) (conj acc (attr-with-opts-or-expr fe-name sym))
                                    (= '* sym) (conj acc sym)
                                    :else (conj acc (map->Field {:id (hash [fe-name sym])
                                                                 :label (keyword->label sym)
                                                                 :attribute sym
                                                                 :cell-data->value sym}))))
                                []
                                pull-pattern)]
    (->> explicit-fields
         (mapcat (fn [field-or-wildcard]
                   (if (= '* field-or-wildcard)
                     (let [explicit-attrs (->> explicit-fields
                                               (remove #(= '* %))
                                               (map #(or (:-alias %) (:attribute %))))]
                       (->> (set/difference (set inferred-attrs) (set explicit-attrs))
                            (sort)
                            (map #(map->Field {:id (hash [fe-name %])
                                               :label (keyword->label %)
                                               :attribute %
                                               :cell-data->value %}))))
                     [field-or-wildcard])))
         (remove #(= :db/id (:attribute %)))
         vec)))

(defn pull-cell->fe [cell source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [splat-attrs (when splat? (keys cell))]
                 (pull->fields pull-pattern splat-attrs fe-name))
       :source-symbol source-symbol
       :splat? splat?
       :type :pull})))

(defn pull-many-cells->fe [column-cells source-symbol fe-name pull-pattern]
  (let [splat? (not (empty? (filter #(= '* %) pull-pattern)))]
    (map->FindElement
      {:name fe-name
       :fields (let [splat-attrs (when splat?
                                   (->> column-cells
                                        (map keys)
                                        (reduce into #{})))]
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
                             :label name
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
    :entity (let [dbname @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/pull-database])
                  source-symbol (symbol dbname)
                  fe-name "entity"
                  pull-pattern @(r/cursor request [:pull-exp])]
              (either/right
                (if-let [a @(r/cursor request [:a])]
                  (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname a :db/cardinality :db/ident])
                    :db.cardinality/one
                    ([(pull-cell->fe @result source-symbol fe-name pull-pattern)])

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
                 datascript.parser.FindTuple (let [result-value @result]
                                               (assert (or (nil? result-value) (vector? result-value)) (str "Expected list, got " (type result-value)))
                                               (->> (:elements qfind)
                                                    (map-indexed (fn [idx element]
                                                                   (auto-fe-one-cell element (get result-value idx))))
                                                    vec))
                 datascript.parser.FindScalar [(auto-fe-one-cell (:element qfind) @result)])))

    :blank (either/right [])

    (either/right [])))
