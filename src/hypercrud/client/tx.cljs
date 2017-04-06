(ns hypercrud.client.tx
  (:require [cljs.core.match :refer-macros [match]]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.util :as util]
            [loom.alg-generic :as loom]))


(defn tempid? [dbid] (< (.-id dbid) 0))


(defn retract [dbid a dbids]
  (map (fn [v] [:db/retract dbid a v]) dbids))

(defn add [dbid a dbids]
  (map (fn [v] [:db/add dbid a v]) dbids))

(defn edit-entity [dbid a rets adds]
  (vec (concat (retract dbid a rets)
               (add dbid a adds))))


(defn update-entity-attr [{:keys [:db/id] :as entity}
                          {:keys [:attribute/ident :attribute/cardinality :attribute/valueType] :as attribute}
                          new-val]
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
                    (conj non-related next-stmt)))))


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
                (case [valueType cardinality]
                  [:db.type/ref :db.cardinality/one] [v]
                  [:db.type/ref :db.cardinality/many] (vec v)
                  []))))
          entity))


(defn entity-and-components->statements [schema e]
  ; tree-seq lets us get component entities too
  (->> (tree-seq map? #(entity-components schema %) e)
       (mapcat entity->statements)))


(defn entity-children [schema entity]
  (mapcat (fn [[attr v]]
            (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
              ; todo can we just check if val is map? or coll? to remove dep on schema?
              ; yes if this is a pulled-tree.
              (case [valueType cardinality]
                [:db.type/ref :db.cardinality/one] [v]
                [:db.type/ref :db.cardinality/many] (vec v)
                [])))
          entity))


(defn pulled-tree->statements [schema pulled-tree]
  (->> (tree-seq map? #(entity-children schema %) pulled-tree)
       (mapcat entity->statements)))


(defn clone-id-factory [conn-id tempid!]
  (let [id-map (atom {})
        replace-id! (fn [dbid]
                      (let [new-dbid (get @id-map dbid)]
                        (if (nil? new-dbid)
                          (let [new-dbid (tempid! conn-id)]
                            (swap! id-map assoc dbid new-dbid)
                            new-dbid)
                          new-dbid)))
        fix-seen-id! (fn [dbid]
                       (get @id-map dbid dbid))]
    [replace-id! fix-seen-id!]))


(defn replace-ids-in-tx [schema conn-id skip? temp-id! tx]
  (let [[replace-id!] (clone-id-factory conn-id temp-id!)]
    (mapv (fn [[op e a v]]
            (let [new-e (replace-id! e)
                  valueType (get-in schema [a :db/valueType])
                  new-v (if (and (not (skip? a)) (= valueType :db.type/ref))
                          (replace-id! v)
                          v)]
              [op new-e a new-v]))
          tx)))

(defn walk-entity [schema f entity]                         ; Walks entity components, applying f to each component, dehydrating non-components
  (->> (f entity)
       (mapv
         (fn [[a v]]
           (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema a)
                 v (if-not (= valueType :db.type/ref)       ; dbid absent from schema, its fine
                     v
                     (if isComponent
                       (case cardinality                    ; Walk component (go deeper)
                         :db.cardinality/one (walk-entity schema f v)
                         :db.cardinality/many (mapv #(walk-entity schema f %) v))

                       (case cardinality                    ; Dehydrate non-component
                         :db.cardinality/one (select-keys v [:db/id])
                         :db.cardinality/many (mapv #(select-keys % [:db/id]) v))))]
             [a v])))
       (into {})))

(defn clone-entity [schema entity tempid!]
  (let [[replace-id! fix-seen-id!] (clone-id-factory (-> entity :db/id :conn-id) tempid!)]
    (->> entity
         ; Walk twice because of cycles.
         (walk-entity schema #(util/update-existing % :db/id replace-id!))
         (walk-entity schema #(util/update-existing % :db/id fix-seen-id!)))))


(defn export-link [schema link]
  (let [tempid! (let [temp-id-atom (atom 0)]
                  (fn [conn-id]
                    (hypercrud.types/->DbId (swap! temp-id-atom dec) conn-id)))
        successors (fn [node]
                     (entity-children schema node))
        filter-pred (fn [node predecessor depth]
                      ;  also may need (not (:db/ident node)) - attrs ref datomic built-ins
                      (not (:attribute/ident node)))]
    (->> (loom/bf-traverse successors link :when filter-pred)
         (mapcat (fn [entity]
                   (if-let [attr (:field/attribute entity)]
                     (let [e (-> (into {} (seq entity))
                                 (dissoc :field/attribute))
                           ; we don't necessarily have the attribute ident pulled down?
                           attr-lookup (->DbId [:attribute/ident (-> attr :attribute/ident)] (-> attr :db/id :conn-id))]
                       (conj (entity->statements e)
                             [:db/add (:db/id e) :field/attribute attr-lookup]))
                     (entity->statements entity))))
         (replace-ids-in-tx schema (-> link :db/id :conn-id)
                            #(contains? #{:field/attribute} %) ; preserve refs to attributes
                            tempid!))))
