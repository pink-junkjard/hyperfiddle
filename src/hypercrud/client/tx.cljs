(ns hypercrud.client.tx
  (:require [cljs.core.match :refer-macros [match]]
            [hypercrud.types :refer [->DbId]]
            [loom.alg-generic :as loom]))


(defn tempid? [dbid] (< (.-id dbid) 0))


(defn retract [dbid a dbids]
  (map (fn [val] [:db/retract dbid a val]) dbids))

(defn add [dbid a dbids]
  (map (fn [val] [:db/add dbid a val]) dbids))

(defn edit-entity [dbid a rets adds]
  (vec (concat (retract dbid a rets)
               (add dbid a adds))))


(defn update-entity-attr [{:keys [:db/id] :as entity}
                          {:keys [:attribute/ident :attribute/cardinality :attribute/valueType] :as attribute}
                          new-val]
  (let [{:keys [old new]} (let [old-val (get entity ident)]
                            (if (= (:db/ident valueType) :db.type/ref)
                              (condp = (:db/ident cardinality)
                                :db.cardinality/one {:old (let [old-val (:db/id old-val)]
                                                            (if (nil? old-val) [] [old-val]))
                                                     :new [new-val]}
                                :db.cardinality/many {:old (map #(:db/id %) old-val)
                                                      :new new-val})
                              (condp = (:db/ident cardinality)
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
    (cond
      (= op :db/add) (if (= op' :db/retract)
                       non-related                          ;we have a related previous stmt that cancels us and it out
                       (conj non-related next-stmt))
      (= op :db/retract) (if (= op' :db/add)
                           non-related                      ;we have a related previous stmt that cancels us and it out
                           (conj non-related next-stmt))
      :else (throw "match error"))))


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

       (mapcat (fn [[attr val]]
                 ; dont care about refs - just pass them through blindly, they are #DbId and will just work
                 (if (vector? val)
                   (mapv (fn [val] [:db/add dbid attr (any-ref->dbid val)]) val)
                   [[:db/add dbid attr (any-ref->dbid val)]])))))


(defn entity-components [schema entity]
  (mapcat (fn [[attr val]]
            (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema attr)]
              (if isComponent
                (cond
                  (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) [val]
                  (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) (vec val)
                  :else []))))
          entity))


(defn entity-children [schema entity]
  (mapcat (fn [[attr val]]
            (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
              (cond
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) [val]
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) (vec val)
                :else [])))
          entity))


(defn replace-ids [schema conn-id skip? temp-id! tx]
  (let [id-map (atom {})
        replace-id! (fn [dbid]
                      (let [new-dbid (get @id-map dbid)]
                        (if (nil? new-dbid)
                          (let [new-dbid (temp-id! conn-id)]
                            (swap! id-map assoc dbid new-dbid)
                            new-dbid)
                          new-dbid)))]
    (mapv (fn [[op e a v]]
            (let [new-e (replace-id! e)
                  valueType (get-in schema [a :db/valueType])
                  new-v (if (and (not (skip? a)) (= valueType :db.type/ref))
                          (replace-id! v)
                          v)]
              [op new-e a new-v]))
          tx)))


(defn clone-entity [schema e tempid!]
  ; tree-seq lets us get component entities too
  (->> (tree-seq map?
                 #(entity-components schema %)
                 e)
       (mapcat entity->statements)
       (replace-ids schema (-> e :db/id :conn-id)
                    (fn [a] (not (get-in schema [a :db/isComponent]))) ; preserve links to existing entities
                    tempid!)))


(defn export-link [schema link tempid!]
  (let [successors (fn [node]
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
         (replace-ids schema (-> link :db/id :conn-id)
                      #(contains? #{:field/attribute} %)    ; preserve refs to attributes
                      tempid!))))
