(ns contrib.datomic
  (:require
    [contrib.data :refer [group-by-pred]]
    [clojure.spec.alpha :as s]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])])
  #?(:clj (:import
            (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
            [clojure.lang IHashEq ILookup])))


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
  (-attr [this a k])
  (-repr-portable-hack [this])
  (cardinality [this a])
  (cardinality? [this a k])
  (cardinality-loose [this a])
  (isComponent [this a])                                   ; follows Datomic naming case conventions
  (valueType [this a])
  (valueType? [this a k])
  (ref? [this a])
  (unique [this a]))

(defn attr-unreverse [a]
  {:pre [(-> (name a) (subs 0 1) (= "_"))]}
  (keyword (namespace a) (-> (name a) (subs 1))))

(deftype Schema [schema-pulledtree schema-by-attr]
  SchemaIndexedNormalized
  (-repr-portable-hack [this] (str "#schema " (pr-str schema-pulledtree)))
  (-attr [this a k]                                         ; Todo assert attribute is found.
    #_(s/assert keyword? a)                                 ; def real assert currenetly failing
    #_(s/assert keyword? k)
    (let [v (smart-lookup-ref-no-tempids
              (get-in schema-by-attr [a k]))]
      #_(s/assert keyword? v)
      v))
  (valueType [this a] (-attr this a :db/valueType))
  (cardinality [this a] (-attr this a :db/cardinality))
  (isComponent [this a] (-attr this a :db/isComponent))
  (unique [this a] (-attr this a :db/unique))
  (valueType? [this a k] (= k (valueType this a)))
  (cardinality? [this a k] (= k (cardinality this a)))
  (cardinality-loose [this a]
    (s/assert keyword? a)
    (let [is-reverse-nav (-> (name a) (subs 0 1) (= "_"))]
      (cond
        (= a :db/id) :db.cardinality/one                    ; :db/id is currently addressable (e.g. user wants to render that column)
        ; unique scalars can't be pulled in reverse
        ; unique ref doesn't imply that _no other attr_ points to the entityvalue
        ; isComponent implies a 1-1 relationship, so the reverse of an isComponent attr will be cardinality-one
        is-reverse-nav (if (isComponent this (attr-unreverse a))
                         :db.cardinality/one
                         :db.cardinality/many)
        :else (cardinality this a))))
  (ref? [this k] (valueType? this k :db.type/ref))

  #?@(:clj
      [Object (equals [o other] (and (instance? Schema other) (= (.-schema-pulledtree o) (.-schema-pulledtree other))))
       IHashEq (hasheq [o] (hash schema-pulledtree))
       ILookup
       (valAt [this k] (get schema-by-attr k))
       (valAt [this k not-found] (get schema-by-attr k not-found))]
      :cljs
      [IEquiv (-equiv [o other] (and (instance? Schema other) (= (.-schema-pulledtree o) (.-schema-pulledtree other))))
       IHash (-hash [o] (hash schema-pulledtree))
       IPrintWithWriter (-pr-writer [o writer _] (-write writer (-repr-portable-hack o)))
       ILookup
       (-lookup [o k] (get schema-by-attr k))
       (-lookup [o k not-found] (get schema-by-attr k not-found))]))

#?(:clj (defmethod print-method Schema [o ^java.io.Writer w] (.write w (-repr-portable-hack o))))
#?(:clj (defmethod print-dup Schema [o w] (print-method o w)))

(defn indexed-schema [schema-pulledtree]
  (Schema. schema-pulledtree (contrib.data/group-by-unique :db/ident schema-pulledtree)))

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
  ;{:pre [(sequential? pull-pattern)]}
  (assert (sequential? pull-pattern) (pr-str pull-pattern))
  (->> pull-pattern
       (map attr-spec->shape)
       (remove nil?)
       vec))

(defn pull-union [& vs]
  ; they have to be the same data-shape (schema), which if this is a valid pull, they are
  (cond
    (sequential? (first vs))
    (let [pull-pattern (apply concat vs)
          [as bs] (group-by-pred map? pull-pattern)]        ; this loses order by rewriting the pull canonical
      (vec
        (concat
          (distinct bs)
          (if-let [as (apply pull-union as)]         ; don't concat [nil]
            [as]))))

    (map? (first vs))
    (apply merge-with pull-union vs)))

(defn tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema pulled-tree]
  {:pre [schema (map? pulled-tree)]}
  (->> pulled-tree
       (reduce-kv
         (fn [acc k v]
           (conj acc (cond
                       (= :db/id k) k
                       (ref? schema k) {k (condp = (cardinality schema k)
                                            :db.cardinality/one (tree-derivative schema v)
                                            :db.cardinality/many (apply pull-union (map (partial tree-derivative schema) v)))}
                       :else k)))
         [])))

(defn pull-enclosure "Union the requested pull-pattern-shape with the actual result shape"
  [schema shape coll]
  {:pre [schema]}
  (apply pull-union shape (map (partial tree-derivative schema) coll)))

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

(defn dimension [schema pullshape pullpath]
  {:pre [schema]}
  (count (filter #(cardinality? schema % :db.cardinality/many))))

(defn ref-one?
  ([schema] (fn [a] (ref-one? schema a)))
  ([schema a]
   {:pre [#_(satisfies? SchemaIndexedNormalized schema)]}
   (and (valueType? schema a :db.type/ref)
        (cardinality? schema a :db.cardinality/one))))

#_(defn downtree-pullpaths [schema pullshape]
  (pull-traverse pullshape (partial ref-one? schema)))

(defn pullpath-unwind-while "Find oldest ancestor matching pred.
  Hint: Pred probably closes over schema."
  [f? pullpath]
  #_(drop-while f? (reverse pullpath))
  (loop [[a & as :as here] (reverse pullpath)]              ; pullpath is guaranteed to align with pullshape
    (if (f? a)
      (recur as)
      here)))

(defn pullshape-get [pullshape a]                        ; arg order is like 'get
  (-> pullshape
      (->> (filter map?))
      (->> (apply merge))
      (get a)))

(defn pullshape-get-in [pullshape as]
  (reduce pullshape-get pullshape as))

(defn reachable-pullpaths [schema root-pullshape pullpath]
  ; Include the one we are at now? There is an off by one in here
  {:pre [schema #_(satisfies? SchemaIndexedNormalized schema)]}
  ; txfn can be on scalar and it is harmless to allow this.
  (let [pred #(cardinality? schema % :db.cardinality/one) #_(ref-one? schema)
        ancestor-path (pullpath-unwind-while pred pullpath)
        ancestor-pull (pullshape-get-in root-pullshape ancestor-path)]
    (pull-traverse ancestor-pull pred)))

(defn reachable-attrs [schema root-pullshape pullpath]
  {:pre [#_(satisfies? SchemaIndexedNormalized schema)]}
  (->> (reachable-pullpaths schema root-pullshape pullpath)
       (map last)
       (remove nil?)))

(defn element-spread [schema {{pull-pattern :value} :pattern :as e} collection]
  ; derivative oriented, ignores spread
  {:pre [schema]}
  (condp = (type e)
    Pull (pull-traverse (pull-enclosure schema (pull-shape pull-pattern) collection))
    Variable [[]]
    Aggregate [[]]))

(defn normalize-result [qfind result]
  ; nil should be treated as empty
  (when result                                              ; unclear if nil result should have been a server error https://github.com/hyperfiddle/hyperfiddle/issues/584
    (condp = (type qfind)
      FindRel result
      FindColl (mapv vector result)
      FindTuple (vector result)
      FindScalar [[result]])))

(defn spread-elements [f schemas qfind result]
  (mapcat (partial f schemas qfind)                         ; (map (partial console-link source)) % has the source
          (range)                                           ; find-element index, for source reversing
          (datascript.parser/find-elements qfind)
          (contrib.data/pad nil (contrib.data/transpose (normalize-result qfind result)))))

(defn pull-level "Get next level e.g. for driving UI forms."
  [pullshape]                                               ; see downtree-pullpaths
  {:pre [(or (sequential? pullshape)
             (nil? pullshape))]}
  (->> pullshape
       (mapcat (fn [attr-spec]
                 (cond
                   (keyword? attr-spec) [attr-spec]
                   (map? attr-spec) (keys attr-spec))))     ; Could verify :ref against schema here
       #_(remove (partial = :db/id))))

(defn result-enclosure "
  no data is not a well-formed result - probably invalid query, but it's less confusing to users
  if the UI still works in this case, since tweaking a formshape does not require the form be populated"
  [schemas qfind ?data]
  {:pre [schemas qfind]
   :post [vector?]}                                         ; associative by index
  (let [?data (contrib.datomic/normalize-result qfind ?data)] ; nil data can mean no query or invalid query, we can still draw forms
    (->> (datascript.parser/find-elements qfind)
         (map-indexed (fn [i element]
                        (condp = (type element)
                          Variable nil
                          Aggregate nil
                          Pull (let [{{db :symbol} :source {pull-pattern :value} :pattern} element
                                     coll (mapv #(get % i) ?data)
                                     schema (get schemas (str db))]
                                 (pull-enclosure schema (pull-shape pull-pattern) coll)))))
         vec)))

(defn spread-elements' [f schemas qfind data]
  (spread-elements (fn [schemas qfind ix e coll-normalized]
                     (let [schema (get schemas (str (get-in e [:source :symbol])))]
                       (f schema e coll-normalized)))
                   schemas qfind data))

(defn qfind-collapse-findrel-1 "Collapse FindRel-1 to FindColl, which simplifies hyperfiddle link biz rules"
  [qfind]
  (let [[e :as es] (datascript.parser/find-elements qfind)]
    (if (= 1 (count es))
      (condp = (type qfind)
        FindRel (datascript.parser/->FindColl e)
        FindTuple (datascript.parser/->FindScalar e)
        FindColl qfind
        FindScalar qfind)
      qfind)))
