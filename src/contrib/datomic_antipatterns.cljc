(ns contrib.datomic-antipatterns
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
    [contrib.data :refer [group-by-pred]]
    [fixtures.ctx :refer [ctx schema result-coll]]))


; This is a graveyard of bad ideas.
; It is helpful to review them the next time we feel a similar need.

; rename: walk-pulled-tree (respecting component)
(defn walk-entity [schema f entity]                         ; Walks entity components, applying f to each component, dehydrating non-components
  (->> (f entity)
       (mapv
         (fn [[a v]]
           (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema a) ; really just for component, the rest could be polymorphic
                 v (if-not (= (:db/ident valueType) :db.type/ref) ; dbid absent from schema, its fine
                     v
                     (if isComponent
                       (case (:db/ident cardinality)        ; Walk component (go deeper)
                         :db.cardinality/one (walk-entity schema f v)
                         :db.cardinality/many (mapv #(walk-entity schema f %) v))

                       (case (:db/ident cardinality)        ; Dehydrate non-component
                         :db.cardinality/one (select-keys v [:db/id])
                         :db.cardinality/many (mapv #(select-keys % [:db/id]) v))))]
             [a v])))
       (into {})))

(defn any-ref->dbid
  "Safe for ref and primitive vals"
  [v]
  (cond (map? v) (:db/id v)
        ; #DbId will just work, its already right
        :else v))

; Internal
(defn ^:legacy entity->statements
  "Only need schema to recurse into component entities. valueType and cardinality is determined dynamically.
  If schema is omitted, don't recurse refs."
  [{dbid :db/id :as entity} & [schema]]
  ;; dissoc :db/id, it is done as a seq/filter for compatibility with #Entity
  (assert (nil? schema) "todo component recursion")
  (->> (seq entity)
       (filter (fn [[k v]] (not= :db/id k)))

       (mapcat (fn [[attr v]]
                 ; dont care about refs - just pass them through blindly, they are #DbId and will just work
                 (if (vector? v)
                   (mapv (fn [v] [:db/add dbid attr (any-ref->dbid v)]) v)
                   [[:db/add dbid attr (any-ref->dbid v)]])))))

(defn ^:legacy entity-components [schema entity]
  (mapcat (fn [[attr v]]
            (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema attr)]
              (if isComponent
                (case [(:db/ident valueType) (:db/ident cardinality)]
                  [:db.type/ref :db.cardinality/one] [v]
                  [:db.type/ref :db.cardinality/many] (vec v)
                  []))))
          entity))

; pulled-tree->statements, respect component
(defn ^:legacy entity-and-components->statements [schema e]
  ; tree-seq lets us get component entities too
  (->> (tree-seq map? #(entity-components schema %) e)
       (mapcat entity->statements)))

; ignores component, not useful imo
(defn ^:legacy pulled-tree-children [schema entity]
  ; Need schema for component. How can you walk a pulled-tree without inspecting component?
  ; If it turns out we don't need component, we can dispatch on map?/coll? and not need schema.
  (mapcat (fn [[attr v]]
            (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
              (case [(:db/ident valueType) (:db/ident cardinality)]
                [:db.type/ref :db.cardinality/one] [v]
                [:db.type/ref :db.cardinality/many] (vec v)
                [])))
          entity))

; ignores component - pretty useless imo
(defn ^:legacy pulled-tree->statements [schema pulled-tree]
  (->> (tree-seq map? #(pulled-tree-children schema %) pulled-tree)
       (mapcat entity->statements)))

; don't know what this is
(defn ^:legacy walk-pulled-tree [schema f tree]                      ; don't actually need schema for anything but component which is ignored here
  (walk/postwalk
    (fn [o] (if (map? o) (f o) o))
    tree))

(defn prewalk-pulled-tree [schema f tree]
  ; Failed experiment for datomic pull parsing
  (->> (f tree)
       (mapv
         (fn [[a v]]
           (let [{{cardinality :db/ident} :db/cardinality
                  {valueType :db/ident} :db/valueType} (get schema a)
                 v (if-not (= :db.type/ref valueType)       ; dbid absent from schema, its fine
                     v
                     (case cardinality
                       :db.cardinality/one (prewalk-pulled-tree schema f v)
                       :db.cardinality/many (mapv #(prewalk-pulled-tree schema f %) v)))]
             [a v])))
       (into {})))

(comment
  (prewalk-pulled-tree schema #(do (println %) %) {:db/id 10 :db/ident :yo})
  (prewalk-pulled-tree schema #(do (println %) %) {:db/id 17592186046209,
                                                   :db/ident :shirt-size/womens-medium,
                                                   :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                                   :reg/gender {:db/id 17592186046204}})
  ;(prewalk-pulled-tree schema distinct pulled-tree-1)
  )