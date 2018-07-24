(ns hypercrud.browser.field
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [contrib.data :refer [transpose]]
            [contrib.reactive :as r]
            [contrib.try$ :refer [try-either]]
            [datascript.parser :as parser]
            [hypercrud.types.Entity :refer [entity?]]))


(defn- last-arg-first [f & args]
  (apply f (last args) (drop-last 1 args)))

(def keyword->label #(some-> % name str))

(defn variable->fe [fe-pos element]
  {::children nil
   ::data-has-id? false
   ::label (str (:symbol element))
   ::get-value (r/partial last-arg-first get fe-pos)
   ::path-segment fe-pos
   ::source-symbol nil})

(let [alias->value (fn [alias cell-data] (get cell-data alias))]
  (defn attr-with-opts-or-expr [is-ref? list-or-vec]
    (if (#{'default 'limit} (first list-or-vec))
      (let [attr (second list-or-vec)]
        {::children nil
         ::data-has-id? (is-ref? attr)
         ::label (keyword->label attr)
         ::get-value attr
         ::path-segment attr
         ::source-symbol nil})
      ; otherwise attr-with-opts
      (let [[attr & {:as opts}] list-or-vec]
        {::-alias (:as opts)
         ::children nil
         ::data-has-id? (is-ref? attr)
         ::label (if-let [alias (:as opts)]
                   (if (string? alias) alias (pr-str alias))
                   (keyword->label attr))
         ::get-value (or (some->> (:as opts) (r/partial alias->value)) attr)
         ::path-segment attr
         ::source-symbol nil}))))

(defn entity-pull? [pull-pattern]
  (boolean (some #{'* :db/id} pull-pattern)))

(defn infer-attrs [data path]
  (let [f (reduce (fn [f segment]
                    (comp
                      (fn [data]
                        (cond
                          (or (map? data) (entity? data)) (get data segment)
                          (or (vector? data) (seq? data)) (map #(get % segment) data)
                          :else nil))
                      f))
                  identity
                  path)
        data-at-path (f data)]
    (->> (cond
           (or (map? data-at-path) (entity? data-at-path)) [data-at-path]
           (or (vector? data-at-path) (seq? data-at-path)) (flatten data-at-path)
           :else nil)
         (mapcat keys)
         (into #{}))))

(defn pull->fields [is-ref? pull-pattern data path]         ; todo inferred attrs needs to walk
  (let [explicit-fields (reduce (fn [acc sym]
                                  ; ::entity? is whether dbid exists while nested
                                  (cond
                                    (map? sym) (->> sym
                                                    (map (fn [[k v]]
                                                           (-> (if (or (vector? k) (seq? k))
                                                                 (attr-with-opts-or-expr is-ref? k)
                                                                 {::label (keyword->label k)
                                                                  ::get-value k
                                                                  ::path-segment k
                                                                  ::source-symbol nil})
                                                               (assoc ::children (when-not (number? v) ; short on recursive-limit https://github.com/hyperfiddle/hyperfiddle/issues/363
                                                                                   (pull->fields is-ref? v data (conj path k)))
                                                                      ::data-has-id? (and (not (number? v)) ; short on recursive-limit https://github.com/hyperfiddle/hyperfiddle/issues/363
                                                                                          (entity-pull? v))))))
                                                    (into acc))
                                    (or (vector? sym) (seq? sym)) (conj acc (attr-with-opts-or-expr is-ref? sym))
                                    (= '* sym) (conj acc sym)
                                    :else (conj acc
                                                {::children nil
                                                 ::data-has-id? (is-ref? sym)
                                                 ::label (keyword->label sym)
                                                 ::get-value sym
                                                 ::path-segment sym
                                                 ::source-symbol nil})))
                                []
                                pull-pattern)]
    (->> explicit-fields
         (mapcat (fn [field-or-wildcard]
                   (if (= '* field-or-wildcard)
                     (let [explicit-attrs (->> explicit-fields
                                               (remove #(= '* %))
                                               (map #(or (::-alias %) (::path-segment %))))]
                       (concat
                         (->> (set/difference (infer-attrs data path) (set explicit-attrs))
                              (sort)
                              (map (fn [attr]
                                     {::children nil
                                      ::data-has-id? false
                                      ::label (keyword->label attr)
                                      ::get-value attr
                                      ::path-segment attr
                                      ::source-symbol nil})))
                         [{::children nil
                           ::data-has-id? false
                           ::label "*"
                           ::get-value (r/constantly nil)
                           ::path-segment '*                ; does this even make sense?
                           ::source-symbol nil}]))
                     [field-or-wildcard])))
         (remove #(= :db/id (::path-segment %)))
         vec)))

(defn build-is-ref? [schemas source-symbol]
  (fn [attr]
    (= :db.type/ref (get-in schemas [(str source-symbol) attr :db/valueType :db/ident]))))

(defn pull-cell->fe [schemas fe-pos cell source-symbol fe-name pull-pattern]
  {::children (pull->fields (build-is-ref? schemas source-symbol) pull-pattern cell [])
   ::data-has-id? (entity-pull? pull-pattern)
   ::get-value (r/partial last-arg-first get fe-pos)
   ::label fe-name
   ::path-segment fe-pos
   ::source-symbol source-symbol})

(defn pull-many-cells->fe [schemas fe-pos column-cells source-symbol fe-name pull-pattern]
  {::children (pull->fields (build-is-ref? schemas source-symbol) pull-pattern column-cells [])
   ::data-has-id? (entity-pull? pull-pattern)
   ::label fe-name
   ::get-value (r/partial last-arg-first get fe-pos)
   ::path-segment fe-pos
   ::source-symbol source-symbol})

(defn aggregate->fe [fe-pos element]
  {::children nil
   ::data-has-id? false
   ::get-value (r/partial last-arg-first get fe-pos)
   ::label (str (cons (get-in element [:fn :symbol])
                      (map #(second (first %))
                           (:args element))))
   ::path-segment fe-pos
   ::source-symbol nil})

(defn auto-fe-one-cell [schemas fe-pos element cell]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe fe-pos element)

    datascript.parser.Pull
    (pull-cell->fe schemas fe-pos cell
                   (get-in element [:source :symbol])
                   (get-in element [:variable :symbol])
                   (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe fe-pos element)))

(defn auto-fe-many-cells [schemas fe-pos element column-cells]
  (condp = (type element)
    datascript.parser.Variable
    (variable->fe fe-pos element)

    datascript.parser.Pull
    (pull-many-cells->fe schemas fe-pos column-cells
                         (get-in element [:source :symbol])
                         (get-in element [:variable :symbol])
                         (get-in element [:pattern :value]))

    datascript.parser.Aggregate
    (aggregate->fe fe-pos element)))

(defn auto-fields [{:keys [:hypercrud.browser/result :hypercrud.browser/request] :as ctx}]
  (let [schemas @(:hypercrud.browser/schemas ctx)]          ; todo tighter reactivity
    (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
      :entity (let [dbname @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/pull-database])
                    source-symbol (symbol dbname)
                    fe-name nil #_"entity"                  ; using "entity" as fe-name makes sense, but is terrible as a label
                    pull-pattern @(r/cursor request [:pull-exp])]
                (either/right
                  (if-let [a @(r/cursor request [:a])]
                    (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname a :db/cardinality :db/ident])
                      :db.cardinality/one
                      [(pull-cell->fe schemas 0 @result source-symbol fe-name pull-pattern)]

                      :db.cardinality/many
                      [(pull-many-cells->fe schemas 0 @result source-symbol fe-name pull-pattern)])
                    [(pull-cell->fe schemas 0 @result source-symbol fe-name pull-pattern)])))

      :query (mlet [{:keys [qfind]} (try-either (parser/parse-query @(r/cursor request [:query])))]
               (cats/return
                 (condp = (type qfind)
                   datascript.parser.FindRel (let [results-by-column (transpose @result)]
                                               (->> (:elements qfind)
                                                    (map-indexed (fn [fe-pos element]
                                                                   (auto-fe-many-cells schemas fe-pos element (get results-by-column fe-pos))))
                                                    vec))
                   datascript.parser.FindColl [(auto-fe-many-cells schemas 0 (:element qfind) @result)]
                   datascript.parser.FindTuple (let [result-value @result]
                                                 (assert (or (nil? result-value) (vector? result-value)) (str "Expected list, got " (type result-value)))
                                                 (->> (:elements qfind)
                                                      (map-indexed (fn [fe-pos element]
                                                                     (auto-fe-one-cell schemas fe-pos element (get result-value fe-pos))))
                                                      vec))
                   datascript.parser.FindScalar [(auto-fe-one-cell schemas 0 (:element qfind) @result)])))

      :blank (either/right [])

      (either/right []))))
