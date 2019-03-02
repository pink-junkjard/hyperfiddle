(ns contrib.datomic
  (:require
    [contrib.data :refer [group-by-pred]]
    [clojure.spec.alpha :as s]
    [clojure.set]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])])
  #?(:clj (:import
            (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
            [clojure.lang IHashEq ILookup])))


(defn tempid? [id]
  (string? id))

(defn identity-segment? [schema attr-spec]                  ; need schema for :identity
  ; Not necessarily a keyword
  ; But accepts pull-shapes, so doesn't have to be the full attr spec
  (contains? #{:db/id :db/ident} attr-spec))

(defn smart-lookup-ref-no-tempids "see hypercrud.browser.context/smart-entity-identifier"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)                ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
      ident
      identity-lookup
      id
      (if-not (map? v) v)                                   ; id-scalar
      nil                                                   ; This is an entity but you didn't pull any identity - error?
      )))

(def parser-types {FindRel ::find-rel
                   FindColl ::find-coll
                   FindTuple ::find-tuple
                   FindScalar ::find-scalar
                   Pull ::pull
                   Variable ::variable
                   Aggregate ::aggregate})

(defn parser-type "element or qfind" [?p]                   ; not reactive
  (parser-types (if ?p
                  (type ?p))))

(defprotocol SchemaIndexedNormalized
  ; Implement this interface in both peer and Hypercrud client
  (-repr-portable-hack [this])
  (attr [this a])
  (attr? [this a corcs])
  (cardinality [this a])
  (cardinality? [this a k])
  (isComponent [this a])                                    ; follows Datomic naming case conventions
  (valueType [this a])
  (valueType? [this a k])
  (ref? [this a])
  (unique [this a])
  (unique? [this a k]))

(defn attr-unreverse [a]
  {:pre [(-> (name a) (subs 0 1) (= "_"))]}
  (keyword (namespace a) (-> (name a) (subs 1))))

(defn make-reverse-attr [schema a]
  ; unique scalars can't be pulled in reverse
  ; unique ref doesn't imply that _no other attr_ points to the entityvalue
  ; isComponent implies a 1-1 relationship, so the reverse of an isComponent attr will be cardinality-one
  {:db/ident a
   :db/valueType :db.type/ref
   :db/cardinality (if (isComponent schema (attr-unreverse a))
                     :db.cardinality/one
                     :db.cardinality/many)})

; :db/id is currently addressable (e.g. user wants to render that column)
(def dbid {:db/ident :db/id
           :db/cardinality :db.cardinality/one
           :db/valueType :db.type/long})

(deftype Schema [schema-pulledtree schema-by-attr]
  #?@(:clj
      [Object (equals [o other] (and (instance? Schema other) (= (.-schema-pulledtree o) (.-schema-pulledtree other))))
       IHashEq (hasheq [o] (hash (.-schema-pulledtree o)))
       ILookup
       (valAt [this k] (get (.-schema-by-attr this) k))
       (valAt [this k not-found] (get (.-schema-by-attr this) k not-found))]
      :cljs
      [IEquiv (-equiv [o other] (and (instance? Schema other) (= (.-schema-pulledtree o) (.-schema-pulledtree other))))
       IHash (-hash [o] (hash (.-schema-pulledtree o)))
       IPrintWithWriter (-pr-writer [o writer _] (-write writer (-repr-portable-hack o)))
       ILookup
       (-lookup [o k] (get (.-schema-by-attr o) k))
       (-lookup [o k not-found] (get (.-schema-by-attr o) k not-found))]))

(extend-protocol SchemaIndexedNormalized
  Schema
  (-repr-portable-hack [this] (str "#schema " (pr-str (.-schema-pulledtree this))))
  (attr [this a]
    (when a
      (s/assert keyword? a)
      (let [is-reverse-nav (-> (name a) (subs 0 1) (= "_"))]
        (cond
          (= a :db/id) dbid
          is-reverse-nav (make-reverse-attr this a)
          :else
          (-> (a (.-schema-by-attr this))
              (contrib.data/update-existing :db/valueType smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/cardinality smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/isComponent smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/unique smart-lookup-ref-no-tempids))))))
  (attr? [this a corcs]
    (let [haystack (into #{} (vals (attr this a)))
          needles (contrib.data/xorxs corcs #{})]
      ; haystack must have all the needles
      (clojure.set/superset? haystack needles)))
  (valueType [this a] (get (attr this a) :db/valueType))
  (cardinality [this a] (get (attr this a) :db/cardinality))
  (isComponent [this a] (get (attr this a) :db/isComponent))
  (unique [this a] (get (attr this a) :db/unique))
  (unique? [this a k] (= k (unique this a)))
  (valueType? [this a k] (= k (valueType this a)))
  (cardinality? [this a k] (= k (cardinality this a)))
  (ref? [this k] (valueType? this k :db.type/ref))

  nil
  (-repr-portable-hack [this] (str "#schema " (pr-str nil)))
  (attr [this a] nil)
  (attr? [this a corcs] false)
  (valueType [this a] nil)
  (cardinality [this a] nil)
  (isComponent [this a] nil)
  (unique [this a] nil)
  (unique? [this a k] nil)
  (valueType? [this a k] nil)
  (cardinality? [this a k] nil)
  (ref? [this k] nil))

#?(:clj (defmethod print-method Schema [o ^java.io.Writer w] (.write w (-repr-portable-hack o))))
#?(:clj (defmethod print-dup Schema [o w] (print-method o w)))

(defn indexed-schema [schema-pulledtree]
  (Schema. schema-pulledtree (contrib.data/group-by-unique :db/ident schema-pulledtree)))

(declare pull-shape)

(defn- attr-spec->shape "Take the pull and get rid of everything, even splat, just attrs requested.
  Does not validate and does not look at schema, do that later."
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
          (if-let [as (apply pull-union as)]                ; don't concat [nil]
            [as]))))

    (map? (first vs))
    (apply merge-with pull-union vs)))

(defn tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema pulled-tree]
  {:pre [schema #_pulled-tree]}
  ; nil pulled-tree is legal, see https://github.com/hyperfiddle/hyperfiddle/issues/298
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
  ; Only do this if the pull contains splat
  (apply pull-union shape (map (partial tree-derivative schema) coll)))

(defn pull-traverse "Manufacture superset of possible link paths
  Collapses {:reg/gender [:db/ident :db/id]} into :reg/gender"
  ([schema pull-shape] (pull-traverse schema pull-shape (constantly true) []))
  ([schema pull-shape pred] (pull-traverse schema pull-shape pred []))
  ([schema pull-shape pred path]                            ; private, internal path accumulator, like a zipper (loop recur would hide this)
   (->> pull-shape
        (mapcat (fn [attr-spec]
                  (cond
                    #_#_(identity-segment? schema attr-spec) ; todo needs schema
                        [[]]                                ; blank path means the element

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
                                                 schema
                                                 children #_(remove #(identity-segment? schema %) children) ; Address entities by :ref
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

(defn pullshape-get [pullshape a]                           ; arg order is like 'get
  (-> pullshape
      (->> (filter map?))
      (->> (apply merge))
      (get a)))

(defn pullshape-get-in [pullshape as]
  (reduce pullshape-get pullshape as))

(defn element-spread [schema {{pull-pattern :value} :pattern :as e} collection]
  ; derivative oriented, ignores spread
  {:pre [schema]}
  (condp = (type e)
    Pull (pull-traverse schema (pull-enclosure schema (pull-shape pull-pattern) collection))
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
  [schemas ?qfind ?data]
  #_{:pre [schemas ?qfind]
     :post [vector?]}                                       ; associative by index
  (when ?qfind
    (let [?data (contrib.datomic/normalize-result ?qfind ?data)] ; nil data can mean no query or invalid query, we can still draw forms
      (->> (datascript.parser/find-elements ?qfind)
           (map-indexed (fn [i element]
                          (condp = (type element)
                            Variable nil
                            Aggregate nil
                            Pull (let [{{db :symbol} :source {pull-pattern :value} :pattern} element
                                       coll (mapv #(get % i) ?data)
                                       dbname (str db)
                                       schema (some-> (get schemas dbname) deref)]
                                   (pull-enclosure schema (pull-shape pull-pattern) coll)))))
           vec))))

(defn spread-elements' [f schemas qfind data]
  (spread-elements (fn [schemas qfind ix e coll-normalized]
                     (let [dbname (str (get-in e [:source :symbol]))
                           schema (some-> (get schemas dbname) deref)]
                       (f schema e coll-normalized)))
                   schemas qfind data))

(defn qfind-collapse-findrel-1 "Collapse FindRel-1 to FindColl, which simplifies the context business rules.
  TODO: also collapse FindRel-N where all elements are aggregates except one?"
  [qfind]
  (let [[e :as es] (datascript.parser/find-elements qfind)]
    (if (= 1 (count es))
      (condp = (type qfind)
        FindRel (datascript.parser/->FindColl e)
        FindTuple (datascript.parser/->FindScalar e)
        FindColl qfind
        FindScalar qfind)
      qfind)))

(defn find-identity-attr "Search an entity map for a :db.unique/identity attribute, or nil if not found.
  Does not traverse :db.type/ref nor :db/isComponent."
  [schema e-map]
  {:pre [schema]}
  (->> (keys e-map)
       (map (juxt identity #(contrib.datomic/unique? schema % :db.unique/identity)))
       (filter second)
       (some first)))

(defn validate-element [schema element _]
  (case (parser-type element)
    ::variable []
    ::aggregate []
    ::pull (let [{{pull-pattern :value} :pattern} element]
             (->> (pull-traverse schema (pull-shape pull-pattern))
                  (remove empty?)
                  (map last)
                  (map (juxt identity #(some-> schema (attr %)))) ; dont crash
                  (filter (comp nil? second))               ; reject good ones
                  (map first)
                  #_empty?)
             #_(tree-seq map?
                         (fn [m]
                           (concat (keys m)
                                   (apply concat (vals m))))
                         (pull-shape pull-pattern))
             #_(->> (pull-traverse (pull-shape pull-pattern))
                    (map last)
                    (mapcat identity))
             ; collect the errors
             )))

(defn validate-qfind-attrs "Validate the pull attributes against schema. Bug: doesn't see Datomic aliases."
  [schemas qfind]
  (if qfind
    (spread-elements' validate-element schemas qfind nil)))
