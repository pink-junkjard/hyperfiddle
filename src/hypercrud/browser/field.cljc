(ns hypercrud.browser.field
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [contrib.data :refer [transpose]]
            [contrib.reactive :as r]
            [contrib.try$ :refer [try-either]]
            [datascript.parser :as parser]
            [hypercrud.types.Entity :refer [entity?]]))


(def keyword->label #(some-> % name str))

(defn is-ref? [schema attr] (= :db.type/ref (get-in schema [attr :db/valueType :db/ident])))
(defn is-component? [schema attr] (boolean (get-in schema [attr :db/isComponent])))

(defn- variable-rel [fe-pos element]
  {::cardinality :db.cardinality/one
   ::children nil
   ::data-has-id? false
   ::label (str (:symbol element))
   ::get-value (r/partial r/last-arg-first get fe-pos)
   ::path-segment fe-pos
   ::source-symbol nil})

(defn- variable [element]
  {::children nil
   ::data-has-id? false
   ::label (str (:symbol element))
   ::get-value identity
   ::path-segment nil
   ::source-symbol nil})

(defn- attr-with-opts-or-expr [schema list-or-vec]
  (if (#{'default 'limit} (first list-or-vec))
    (let [attr (second list-or-vec)]
      {::cardinality (get-in schema [attr :db/cardinality :db/ident])
       ::children nil
       ::data-has-id? (is-ref? schema attr)
       ::label (keyword->label attr)
       ::get-value attr
       ::path-segment attr
       ::source-symbol nil})
    ; otherwise attr-with-opts
    (let [[attr & {:as opts}] list-or-vec]
      {::-alias (:as opts)
       ::cardinality (get-in schema [attr :db/cardinality :db/ident])
       ::children nil
       ::data-has-id? (is-ref? schema attr)
       ::label (if-let [alias (:as opts)]
                 (if (string? alias) alias (pr-str alias))
                 (keyword->label attr))
       ::get-value (or (some->> (:as opts) (r/partial r/last-arg-first get)) attr)
       ::path-segment attr
       ::source-symbol nil})))

(defn entity-pull? [pull-pattern]
  (boolean (some #{'* :db/id} pull-pattern)))

(defn infer-attrs [data get-values]
  (let [f (reduce (fn [f get-value]
                    (comp
                      (fn [data]
                        (cond
                          (or (map? data) (entity? data)) (get-value data)
                          (or (vector? data) (seq? data)) (reduce (fn [acc data]
                                                                    (let [sub-data (get-value data)]
                                                                      (if (or (vector? sub-data) (seq? sub-data))
                                                                        (concat acc sub-data)
                                                                        (conj acc sub-data))))
                                                                  []
                                                                  data)
                          :else nil))
                      f))
                  identity
                  get-values)
        data-at-path (f data)]
    (->> (cond
           (or (map? data-at-path) (entity? data-at-path)) [data-at-path]
           (or (vector? data-at-path) (seq? data-at-path)) data-at-path
           :else nil)
         (mapcat keys)
         (into #{}))))

(defn- pull->fields [schema pull-pattern data get-values]
  (let [explicit-fields (reduce (fn [acc sym]
                                  (cond
                                    (map? sym) (->> sym
                                                    (map (fn [[k v]]
                                                           (let [field (if (or (vector? k) (seq? k))
                                                                         (attr-with-opts-or-expr is-ref? k)
                                                                         {::cardinality (get-in schema [k :db/cardinality :db/ident])
                                                                          ::label (keyword->label k)
                                                                          ::get-value k
                                                                          ::path-segment k
                                                                          ::source-symbol nil})]
                                                             (assoc field
                                                               ::children (when-not (number? v) ; short on recursive-limit https://github.com/hyperfiddle/hyperfiddle/issues/363
                                                                            (pull->fields schema v data (conj get-values (::get-value field))))
                                                               ::data-has-id? (and (not (number? v)) ; short on recursive-limit https://github.com/hyperfiddle/hyperfiddle/issues/363
                                                                                   (entity-pull? v))))))
                                                    (into acc))
                                    (or (vector? sym) (seq? sym)) (let [field (attr-with-opts-or-expr schema sym)]
                                                                    (->> (if (is-component? schema (::path-segment field))
                                                                           (assoc field ::children (pull->fields schema [::implicit-splat] data (conj get-values (::get-value field))))
                                                                           field)
                                                                         (conj acc)))
                                    (= '* sym) (conj acc sym)
                                    (= ::implicit-splat sym) (conj acc sym)
                                    :else (conj acc
                                                {::cardinality (get-in schema [sym :db/cardinality :db/ident])
                                                 ::children (when (is-component? schema sym)
                                                              (pull->fields schema [::implicit-splat] data (conj get-values sym)))
                                                 ::data-has-id? (is-ref? schema sym)
                                                 ::label (keyword->label sym)
                                                 ::get-value sym
                                                 ::path-segment sym
                                                 ::source-symbol nil})))
                                []
                                pull-pattern)]
    (->> explicit-fields
         (mapcat (fn [field-or-wildcard]
                   (if (#{'* ::implicit-splat} field-or-wildcard)
                     (let [explicit-attrs (->> explicit-fields
                                               (remove #(= '* %))
                                               (map #(or (::-alias %) (::path-segment %))))]
                       (concat
                         (->> (set/difference (infer-attrs data get-values) (set explicit-attrs))
                              (sort)
                              (map (fn [attr]
                                     {::cardinality (get-in schema [attr :db/cardinality :db/ident])
                                      ::children (when (is-component? schema attr)
                                                   (pull->fields schema [::implicit-splat] data (conj get-values attr)))
                                      ::data-has-id? (is-ref? schema attr)
                                      ::label (keyword->label attr)
                                      ::get-value attr
                                      ::path-segment attr
                                      ::source-symbol nil})))
                         (when (= '* field-or-wildcard)
                           [{::cardinality :db.cardinality/one
                             ::children nil
                             ::data-has-id? false
                             ::label "*"
                             ::get-value (r/constantly nil)
                             ::path-segment '*              ; does this even make sense?
                             ::source-symbol nil}])))
                     [field-or-wildcard])))
         (remove #(= :db/id (::path-segment %)))
         vec)))

(defn- pull-one [schemas cell source-symbol label pull-pattern]
  {::cardinality :db.cardinality/one
   ::children (pull->fields (get schemas (str source-symbol)) pull-pattern cell [])
   ::data-has-id? (entity-pull? pull-pattern)
   ::get-value identity
   ::label label
   ::path-segment nil
   ::source-symbol source-symbol})

(defn- aggregate-rel [fe-pos element]
  {::cardinality :db.cardinality/one
   ::children nil
   ::data-has-id? false
   ::get-value (r/partial r/last-arg-first get fe-pos)
   ::label (str (cons (get-in element [:fn :symbol])
                      (map #(second (first %))
                           (:args element))))
   ::path-segment fe-pos
   ::source-symbol nil})

(defn- aggregate [element]
  {::cardinality :db.cardinality/one
   ::children nil
   ::data-has-id? false
   ::get-value identity
   ::label (str (cons (get-in element [:fn :symbol])
                      (map #(second (first %))
                           (:args element))))
   ::path-segment nil
   ::source-symbol nil})

(defn auto-field [request {:keys [:hypercrud.browser/data] :as ctx}]
  (let [schemas @(:hypercrud.browser/schemas ctx)]          ; todo tighter reactivity
    (case @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/type])
      :entity (let [dbname @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/pull-database])
                    source-symbol (symbol dbname)
                    label nil #_"entity"                    ; "entity" is terrible as a label
                    pull-pattern @(r/cursor request [:pull-exp])]
                (either/right (pull-one schemas @data source-symbol label pull-pattern)))

      :query (mlet [{:keys [qfind]} (try-either (parser/parse-query @(r/cursor request [:query])))]
               (cats/return
                 (condp = (type qfind)
                   datascript.parser.FindRel
                   (let [results-by-column (transpose @data)]
                     {::cardinality :db.cardinality/many
                      ::children (->> (:elements qfind)
                                      (map-indexed (fn [fe-pos element]
                                                     (condp = (type element)
                                                       datascript.parser.Variable
                                                       (variable-rel fe-pos element)

                                                       datascript.parser.Pull
                                                       (let [source-symbol (get-in element [:source :symbol])
                                                             pull-pattern (get-in element [:pattern :value])]
                                                         {::cardinality :db.cardinality/one
                                                          ::children (pull->fields (get schemas (str source-symbol)) pull-pattern (get results-by-column fe-pos) [])
                                                          ::data-has-id? (entity-pull? pull-pattern)
                                                          ::get-value (r/partial r/last-arg-first get fe-pos)
                                                          ::label (get-in element [:variable :symbol])
                                                          ::path-segment fe-pos
                                                          ::source-symbol source-symbol})

                                                       datascript.parser.Aggregate
                                                       (aggregate-rel fe-pos element))))
                                      vec)
                      ::data-has-id? false
                      ::get-value identity
                      ::label nil
                      ::path-segment nil
                      ::source-symbol nil})

                   datascript.parser.FindColl
                   (condp = (type (:element qfind))
                     datascript.parser.Variable
                     (-> (variable (:element qfind))
                         (assoc ::cardinality :db.cardinality/many))

                     datascript.parser.Pull
                     (let [source-symbol (get-in qfind [:element :source :symbol])
                           pull-pattern (get-in qfind [:element :pattern :value])]
                       {::cardinality :db.cardinality/many
                        ::children (pull->fields (get schemas (str source-symbol)) pull-pattern @data [])
                        ::data-has-id? (entity-pull? pull-pattern)
                        ::get-value identity
                        ::label (get-in qfind [:element :variable :symbol])
                        ::path-segment nil
                        ::source-symbol source-symbol})

                     datascript.parser.Aggregate
                     (-> (aggregate (:element qfind))
                         (assoc ::cardinality :db.cardinality/many)))

                   datascript.parser.FindTuple
                   {::cardinality :db.cardinality/one
                    ::children (->> (:elements qfind)
                                    (map-indexed (fn [fe-pos element]
                                                   (condp = (type element)
                                                     datascript.parser.Variable
                                                     (variable-rel fe-pos element)

                                                     datascript.parser.Pull
                                                     (let [source-symbol (get-in element [:source :symbol])
                                                           pull-pattern (get-in element [:pattern :value])]
                                                       {::cardinality :db.cardinality/one
                                                        ::children (pull->fields (get schemas (str source-symbol)) pull-pattern (get @data fe-pos) [])
                                                        ::data-has-id? (entity-pull? pull-pattern)
                                                        ::get-value (r/partial r/last-arg-first get fe-pos)
                                                        ::label (get-in element [:variable :symbol])
                                                        ::path-segment fe-pos
                                                        ::source-symbol source-symbol})

                                                     datascript.parser.Aggregate
                                                     (aggregate-rel fe-pos element))))
                                    vec)
                    ::data-has-id? false
                    ::get-value identity
                    ::label nil
                    ::path-segment nil
                    ::source-symbol nil}

                   datascript.parser.FindScalar
                   (condp = (type (:element qfind))
                     datascript.parser.Variable
                     (-> (variable (:element qfind))
                         (assoc ::cardinality :db.cardinality/one))

                     datascript.parser.Pull
                     (pull-one schemas @data
                               (get-in qfind [:element :source :symbol])
                               (get-in qfind [:element :variable :symbol])
                               (get-in qfind [:element :pattern :value]))

                     datascript.parser.Aggregate
                     (-> (aggregate (:element qfind))
                         (assoc ::cardinality :db.cardinality/one))))))

      :blank (either/right [])

      (either/right []))))
