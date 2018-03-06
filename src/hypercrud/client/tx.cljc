(ns hypercrud.client.tx
  (:require [clojure.walk :as walk]))


(defn retract [id a ids]
  (map (fn [v] [:db/retract id a v]) ids))

(defn add [id a ids]
  (map (fn [v] [:db/add id a v]) ids))

(defn edit-entity [id a rets adds]
  {:pre [id a]}
  (vec (concat (retract id a rets)
               (add id a adds))))

; this fn blasts away previous values
; in cardinality/many case you may not want that behaviour
(defn update-entity-attr [{:keys [:db/id] :as entity}
                          {:keys [:db/ident :db/cardinality :db/valueType] :as attribute}
                          new-val]
  {:pre [#_(not (js/isNaN new-val)) #_"What if new-val is nil or ##NaN etc? #88"
         (not (nil? new-val))]}
  (let [{:keys [old new]} (let [old-val (get entity ident)]
                            (if (= (:db/ident valueType) :db.type/ref)
                              (case (:db/ident cardinality)
                                :db.cardinality/one {:old (let [old-val (:db/id old-val)]
                                                            (if (nil? old-val) [] [old-val]))
                                                     :new [new-val]}
                                :db.cardinality/many {:old (map #(:db/id %) old-val)
                                                      :new new-val})
                              (case (:db/ident cardinality)
                                :db.cardinality/one {:old (if (nil? old-val) [] [old-val])
                                                     :new [new-val]}
                                :db.cardinality/many {:old old-val
                                                      :new new-val})))]
    (edit-entity id ident old new)))

(defn simplify [simplified-tx next-stmt]
  (let [[op e a v] next-stmt
        g (group-by (fn [[op' e' a' v']] (and (= e' e) (= a' a) (= v' v)))
                    simplified-tx)
        [op' e' a' v'] (first (get g true))                 ;if this count > 1, we have duplicate stmts, they are harmless and discard dups here.
        non-related (get g false)]
    (case op
      :db/add (if (= op' :db/retract)
                non-related                                 ;we have a related previous stmt that cancels us and it out
                (conj non-related next-stmt))
      :db/retract (if (= op' :db/add)
                    non-related                             ;we have a related previous stmt that cancels us and it out
                    (conj non-related next-stmt))

      ; else probably a :db.fn
      (conj non-related next-stmt))))

(defn into-tx [tx more-statements]
  "We don't care about the cardinality (schema) because the UI code is always
  retracting values before adding new value, even in cardinality one case. This is a very
  convenient feature and makes the local datoms cancel out properly always to not cause
  us to re-assert datoms needlessly in datomic"
  (reduce simplify tx more-statements))

(defn any-ref->dbid
  "Safe for ref and primitive vals"
  [v]
  (cond (map? v) (:db/id v)
        ; #DbId will just work, its already right
        :else v))

; Internal
(defn entity->statements
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

(defn entity-components [schema entity]
  (mapcat (fn [[attr v]]
            (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema attr)]
              (if isComponent
                (case [(:db/ident valueType) (:db/ident cardinality)]
                  [:db.type/ref :db.cardinality/one] [v]
                  [:db.type/ref :db.cardinality/many] (vec v)
                  []))))
          entity))

; pulled-tree->statements, respect component
(defn entity-and-components->statements [schema e]
  ; tree-seq lets us get component entities too
  (->> (tree-seq map? #(entity-components schema %) e)
       (mapcat entity->statements)))

; ignores component, not useful imo
(defn pulled-tree-children [schema entity]
  ; Need schema for component. How can you walk a pulled-tree without inspecting component?
  ; If it turns out we don't need component, we can dispatch on map?/coll? and not need schema.
  (mapcat (fn [[attr v]]
            (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
              (case [(:db/ident valueType) (:db/ident cardinality)]
                [:db.type/ref :db.cardinality/one] [v]
                [:db.type/ref :db.cardinality/many] (vec v)
                [])))
          entity))

; ignore component - pretty useless imo
(defn pulled-tree->statements [schema pulled-tree]
  (->> (tree-seq map? #(pulled-tree-children schema %) pulled-tree)
       (mapcat entity->statements)))

; don't know what this is
(defn walk-pulled-tree [schema f tree]                      ; don't actually need schema for anything but component which is ignored here
  (walk/postwalk
    (fn [o] (if (map? o) (f o) o))
    tree))

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

(letfn [(update-v [id->tempid schema a v]
          (if (= :db.type/ref (get-in schema [a :db/valueType :db/ident]))
            (get id->tempid v v)
            v))
        (add-ret [id->tempid schema [op e a v]]
          (let [e (get id->tempid e e)
                v (update-v id->tempid schema a v)]
            [op e a v]))
        (retractEntity [id->tempid schema [op e]]
          [op (get id->tempid e e)])
        (cas [id->tempid schema [op e a ov nv]]
          [op
           (get id->tempid e e)
           a
           (update-v id->tempid schema a ov)
           (update-v id->tempid schema a nv)])]
  (defn stmt-id->tempid [id->tempid schema [op :as stmt]]
    (let [f (case op
              :db/add add-ret
              :db/retract add-ret
              :db/retractEntity retractEntity
              :db.fn/retractEntity retractEntity
              :db/cas cas
              :db.fn/cas cas)]
      (f id->tempid schema stmt))))
