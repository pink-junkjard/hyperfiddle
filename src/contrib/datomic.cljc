(ns contrib.datomic
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [contrib.data :refer [group-by-pred]]))


(defn valueType [schema-by-attr k]
  ; :db/id is nil
  (-> schema-by-attr (get k) :db/valueType :db/ident))

(defn ref? [schema-by-attr k]                               ; Nasty treeish client code. Should this use dynamic scope to hide schema?
  {:pre [schema-by-attr (keyword? k)]}
  (-> schema-by-attr (get k) :db/valueType :db/ident (= :db.type/ref)))

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
  [schema pull-pattern-shape coll]
  (apply pull-shape-union pull-pattern-shape (map (partial pulled-tree-derivative schema) coll)))

(comment
  (infer-attrs result identity)
  (apply into (->> (map tree-derivative result) (map set)))
  (transpose result)                                        ; for relations only
  )

(defn form-traverse [form-shape & [path]]
  (->> form-shape
       (mapcat (fn [attr-spec]
                 (cond
                   (keyword? attr-spec) [(conj (vec path) attr-spec)]
                   (map? attr-spec) (->> attr-spec
                                         (mapcat (fn [[k children]]
                                                   (let [path (conj (vec path) k)]
                                                     (concat [path]
                                                             (form-traverse children path)))))))))))

