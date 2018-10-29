(ns contrib.datomic
  (:require
    [contrib.data :refer [group-by-pred]]))


(defn identity-segment? [attr-spec]
  ; Not necessarily a keyword
  ; But accepts pull-shapes, so doesn't have to be the full attr spec
  (contains? #{:db/id :db/ident} attr-spec))

(defn smart-lookup-ref-no-tempids "see hyperfiddle.tempid/smart-entity-identifier"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)                  ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
        ident
        identity-lookup
        id
        (if-not (map? v) v)                                 ; id-scalar
        nil                                                 ; This is an entity but you didn't pull any identity - error?
        )))

(defn valueType [schema-by-attr k]
  ; :db/id is nil
  (-> schema-by-attr (get k) :db/valueType smart-lookup-ref-no-tempids))

(defn ref? [schema-by-attr k]                               ; Nasty treeish client code. Should this use dynamic scope to hide schema?
  {:pre [schema-by-attr (keyword? k)]}
  (-> schema-by-attr (get k) :db/valueType smart-lookup-ref-no-tempids (= :db.type/ref)))

(declare pull-shape)

(defn- attr-spec->shape "Take the pull and get rid of everything, even splat, just attrs requested."
  [a]
  ; todo :as
  (cond
    (keyword? a) a
    (#{'* "*"} a) nil
    (map? a) (reduce-kv (fn [m k v]
                          (assoc m (attr-spec->shape k)
                                   (pull-shape v)))
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

(defn pulled-tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema-by-attr pulled-tree]
  {:pre [schema-by-attr (map? pulled-tree)]}
  (let [ref? (partial ref? schema-by-attr)]
    (->> pulled-tree
         (reduce-kv
           (fn [acc k v]
             (conj acc (cond
                         (= :db/id k) k
                         (ref? k) {k (pulled-tree-derivative schema-by-attr v)}
                         :else k)))
           []))))

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

