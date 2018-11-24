(ns contrib.datomic
  (:require
    [contrib.data :refer [group-by-pred]]
    [contrib.platform]
    [clojure.walk :refer [prewalk]]
    ;#?(:clj [datomic.api :as d]) ; breaks cljs compile-time macros
    #?(:clj [loom.alg-generic])
    ))


#?(:clj
   (contrib.platform/code-for-jvm
     (require '[datomic.api])))

(defn identity-segment? [attr-spec]
  ; Not necessarily a keyword
  ; But accepts pull-shapes, so doesn't have to be the full attr spec
  (contains? #{:db/id :db/ident} attr-spec))

(defn smart-lookup-ref-no-tempids "see hyperfiddle.tempid/smart-entity-identifier"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)                ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
      ident
      identity-lookup
      id
      (if-not (map? v) v)                                   ; id-scalar
      nil                                                   ; This is an entity but you didn't pull any identity - error?
      )))

(defn valueType [schema-by-attr k]
  {:pre [(map? schema-by-attr) (keyword? k)]}
  ; :db/id is nil
  (-> schema-by-attr (get k) :db/valueType smart-lookup-ref-no-tempids))

#?(:clj
   (defn ref? [$ k]
     (= :db.type/ref (->> k (datomic.api/entity $) :db/valueType))))

(defn ref?' [schema k]                                      ; Nasty treeish client code. Should this use dynamic scope to hide schema?
  {:pre [(map? schema) (keyword? k)]}
  (= :db.type/ref (valueType schema k)))

#?(:clj
   (defn one? [$ k]
     (= :db.cardinality/one (->> k (datomic.api/entity $) :db/cardinality))))

#?(:clj
   (defn many? [$ k]
     (= :db.cardinality/many (->> k (datomic.api/entity $) :db/cardinality))))

#?(:clj
   (defn component? [$ k]
     (->> k (datomic.api/entity $) :db/isComponent)))

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
          (if-let [as (apply pull-shape-union as)]          ; don't concat [nil]
            [as]))))

    (map? (first vs))
    (apply merge-with pull-shape-union vs)))

(defn pulled-tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema pulled-tree]
  {:pre [(map? schema) (map? pulled-tree)]}
  (let [ref? (partial ref?' schema)]
    (->> pulled-tree
         (reduce-kv
           (fn [acc k v]
             (conj acc (cond
                         (= :db/id k) k
                         (ref? k) {k (condp = (smart-lookup-ref-no-tempids (:db/cardinality (schema k)))
                                       :db.cardinality/one (pulled-tree-derivative schema v)
                                       :db.cardinality/many (apply pull-shape-union (map (partial pulled-tree-derivative schema) v)))}
                         :else k)))
           []))))

(defn enclosing-pull-shape "Union the requested pull-pattern-shape with the actual result shape"
  [schema shape coll]
  (apply pull-shape-union shape (map (partial pulled-tree-derivative schema) coll)))

(defn pull-traverse "Manufacture superset of possible link paths"
  [pull-shape & [path]]
  (->> pull-shape
       (mapcat (fn [attr-spec]
                 (cond
                   (identity-segment? attr-spec) [[]]       ; top level only
                   (keyword? attr-spec) [(conj (vec path) attr-spec)]
                   (map? attr-spec) (->> attr-spec
                                         (mapcat (fn [[ref-attr children]]
                                                   (let [path (conj (vec path) ref-attr)]
                                                     (concat [path]
                                                             (pull-traverse (remove identity-segment? children) path)))))
                                         distinct))))
       distinct))

(defn tempids [es]
  (zipmap es (->> (iterate inc 0) (map str))))

; Cloning an entity is shallow - it includes components, but not connections
; Forking a subgraph makes a copy, including all connections.

#?(:clj
   (defn datomic-entity-successors [$ e]
     (->> (-> (datomic.api/pull $ ['*] e) (dissoc :db/id))
          (mapcat (fn [[k v]]
                    (if (and (ref? $ k) (not (component? $ k)))
                      (cond (one? $ k) [(:db/id v)]
                            (many? $ k) (mapv :db/id v))

                      ))))))

(defn alter-ids [m #_":: id -> tempid" tree]
  (prewalk (fn [tree]
             (if (map? tree)
               (if-let [id (m (:db/id tree))]
                 (assoc tree :db/id id)
                 (dissoc tree :db/id))
               tree))
           tree))

#?(:clj
   (defn connected-entities [$ es]
     (->> es (mapcat #(loom.alg-generic/bf-traverse (partial datomic-entity-successors $) %)) set)))

#?(:clj
   (defn clone-entities
     "Shallow clone a Datomic entity and any reachable entities by walking the entity graph breadth-first with Loom."
     [$ es]
     (let [xs (connected-entities $ es)]
       (->> (seq xs)
            (datomic.api/pull-many $ ['*])
            (mapv (partial alter-ids (tempids xs)))))))

;(defn lookup-id [lookup id]
;  (if-let [tid (get-in lookup [:lookup id])]
;    [lookup tid]
;    (let [tid (str id)]
;      [(assoc-in lookup [:lookup id] tid)
;       tid])))

(defn attr-datomic? [$ e-attr]
  (<= e-attr 62))

#?(:clj
   (defn export-schema [$]
     (->> (datomic.api/q '[:find [?attr ...] :where
                           [:db.part/db :db.install/attribute ?attr]
                           [(> ?attr 62)]] $)
          (datomic.api/pull-many $ ['*])
          (sort-by :db/ident))))

#?(:clj
   (defn entity-creation-tx [$ e]
     (->> (datomic.api/q '[:find (min ?tx) .
                           :in $ ?e
                           :where
                           [?e _ _ ?tx]]
                         $ e))))

(comment
  (def $ (db! "datomic:free://datomic:4334/hyperfiddle-users"))
  (->> (datomic.api/q
         '[:find
           ?name
           ?t
           :where
           [?user :user/user-id]
           [?user :user/name ?name]
           [(user/entity-creation-tx $ ?user) ?t]]
         $)
       (sort-by first))
  )
