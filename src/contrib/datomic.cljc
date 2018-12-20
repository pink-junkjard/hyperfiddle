(ns contrib.datomic
  (:require
    [contrib.data :refer [group-by-pred]]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])])
  #?(:clj (:import
            (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
            [clojure.lang #_#_#_Counted IFn IHashEq ILookup #_Seqable]
            [clojure.core.protocols IKVReduce])))


(defn tempid? [id]
  (string? id))

(defn identity-segment? [attr-spec]
  ; Not necessarily a keyword
  ; But accepts pull-shapes, so doesn't have to be the full attr spec
  (contains? #{:db/id :db/ident} attr-spec))

(defn smart-lookup-ref-no-tempids "see hypercrud.browser.context/smart-entity-identifier"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)                  ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
        ident
        identity-lookup
        id
        (if-not (map? v) v)                                 ; id-scalar
        nil                                                 ; This is an entity but you didn't pull any identity - error?
        )))

(defn consistent-relation-key "Key that the dbval has, but unstable in views with a branch"
  [v]
  (or (smart-lookup-ref-no-tempids v) v))

(defprotocol SchemaIndexedNormalized
  ; Implement this interface in both peer and Hypercrud client
  (-attr [_ a k])
  (valueType [_ a])
  (valueType? [_ a k])
  (cardinality [_ a])
  (cardinality? [_ a k])
  (ref? [_ attr]))

(defn indexed-schema [schema]
  (let [schema-by-attr (contrib.data/group-by-unique :db/ident schema)]
    (reify
      SchemaIndexedNormalized
      (-attr [this a k]
        {:pre [(keyword? a) (keyword? k)]}
        (smart-lookup-ref-no-tempids
          (get-in schema-by-attr [a k])))
      (valueType [this a] (-attr this a :db/valueType))
      (cardinality [this a] (-attr this a :db/cardinality))
      (valueType? [this a k] (= k (valueType this a)))
      (cardinality? [this a k] (= k (valueType this a)))
      (ref? [this k] (valueType? this k :db.type/ref))

      ILookup
      (valAt [this k] (get schema-by-attr k))
      (valAt [this k not-found] (get schema-by-attr k not-found)))))

(declare pull-shape)

(defn- attr-spec->shape "Take the pull and get rid of everything, even splat, just attrs requested."
  [a]
  ; todo :as
  (cond
    (keyword? a) a
    (#{'* "*"} a) nil
    (map? a) (reduce-kv (fn [m k v]
                          (when-not (number? v)             ; bail on recursive specs https://github.com/hyperfiddle/hyperfiddle/issues/363
                            (assoc m (attr-spec->shape k)
                                     (pull-shape v))))
                        {} a)
    (and (sequential? a) (keyword? (first a))) (first a)
    (and (sequential? a) (#{'limit "limit" 'default "default"} (first a))) (second a)
    :else a))

(defn pull-shape [pull-pattern]
  {:pre [(sequential? pull-pattern)]}
  (->> pull-pattern
       (map attr-spec->shape)
       (remove nil?)
       vec))

(defn pull-shape-union [& vs]
  ; they have to be the same data-shape, which if this is a valid pull, they are
  (cond
    (sequential? (first vs))
    (let [pull-pattern (apply concat vs)
          [as bs] (group-by-pred map? pull-pattern)]        ; this loses order by rewriting the pull canonical
      (vec
        (concat
          (distinct bs)
          (if-let [as (apply pull-shape-union as)]         ; don't concat [nil]
            [as]))))

    (map? (first vs))
    (apply merge-with pull-shape-union vs)))

(defn pulled-tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema pulled-tree]
  {:pre [(map? pulled-tree)]}
  (->> pulled-tree
       (reduce-kv
         (fn [acc k v]
           (conj acc (cond
                       (= :db/id k) k
                       (ref? schema k) {k (condp = (cardinality schema k)
                                            :db.cardinality/one (pulled-tree-derivative schema v)
                                            :db.cardinality/many (apply pull-shape-union (map (partial pulled-tree-derivative schema) v)))}
                       :else k)))
         [])))

(defn enclosing-pull-shape "Union the requested pull-pattern-shape with the actual result shape"
  [schema shape coll]
  (apply pull-shape-union shape (map (partial pulled-tree-derivative schema) coll)))

(defn pull-traverse "Manufacture superset of possible link paths
  Collapses {:reg/gender [:db/ident :db/id]} into :reg/gender"
  ([pull-shape] (pull-traverse pull-shape (constantly true) []))
  ([pull-shape pred] (pull-traverse pull-shape pred []))
  ([pull-shape pred path] ; private, internal path accumulator, like a zipper (loop recur would hide this)
   (->> pull-shape
        (mapcat (fn [attr-spec]
                  (cond
                    (identity-segment? attr-spec)
                    [[]]                                    ; blank path means the element

                    (keyword? attr-spec)                    ; scalars, never refs
                    (if (pred attr-spec)
                      [(conj (vec path) attr-spec)])

                    (map? attr-spec)
                    (->> attr-spec
                         (mapcat (fn [[ref-attr children]]  ; always refs
                                   (if (pred ref-attr)
                                     (let [path (conj (vec path) ref-attr)]
                                       (concat [path]
                                               ; Collapse pulled identity into parent from link's perspective
                                               ; :unique :identity should too todo
                                               (pull-traverse
                                                 (remove identity-segment? children) ; Address entities by :ref
                                                 pred
                                                 path))))))
                         distinct))))
        distinct)))                                         ; why distinct? This breaks tail recursion

(defn element-spread [schema {{pull-pattern :value} :pattern :as e} collection]
  (condp = (type e)
    Pull (pull-traverse (enclosing-pull-shape schema (pull-shape pull-pattern) collection))
    Variable [[]]
    Aggregate [[]]))

(defn normalize-result [qfind result]
  (when result                                              ; unclear if nil result should have been a server error https://github.com/hyperfiddle/hyperfiddle/issues/584
    (condp = (type qfind)
      FindColl (mapv vector result)
      FindRel result
      FindTuple (mapv vector result)
      FindScalar [[result]])))

(defn spread-elements [f schemas qfind result]
  (mapcat (partial f schemas qfind)                         ; (map (partial console-link source)) % has the source
          (range)                                           ; find-element index, for source reversing
          (datascript.parser/find-elements qfind)
          (contrib.data/pad nil (contrib.data/transpose (normalize-result qfind result)))))
