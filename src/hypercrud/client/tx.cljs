(ns hypercrud.client.tx
  (:require [hypercrud.types :refer [->DbId]]))


(defn tempid? [dbid] (< (.-id dbid) 0))


(defn edit-entity [dbid a rets adds]
  (vec (concat (map (fn [val] [:db/retract dbid a val]) rets)
               (map (fn [val] [:db/add dbid a val]) adds))))


(defn update-entity-card-one-attr [{:keys [:db/id] :as entity} a new-val]
  (edit-entity id a [(get entity a)] [new-val]))


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


(defn apply-stmt-to-entity [schema entity [op _ a v]]
  (let [cardinality (get-in schema [a :db/cardinality])
        _ (assert cardinality (str "schema attribute not found: " (pr-str a)))]
    (cond
      (and (= op :db/add) (= cardinality :db.cardinality/one)) (assoc entity a v)
      (and (= op :db/retract) (= cardinality :db.cardinality/one)) (dissoc entity a)
      (and (= op :db/add) (= cardinality :db.cardinality/many)) (update-in entity [a] (fnil #(conj % v) #{}))
      (and (= op :db/retract) (= cardinality :db.cardinality/many)) (update-in entity [a] (fnil #(disj % v) #{}))
      :else (throw "match error"))))


(defn build-entity-lookup
  ([schema statements dbval] (build-entity-lookup schema statements dbval {}))
  ([schema statements dbval lookup]
   (reduce (fn [lookup [op dbid a v]]
             (update lookup dbid (fn [entity]
                                   (let [entity (or entity (with-meta {:db/id dbid} {:dbval dbval}))]
                                     (apply-stmt-to-entity schema entity [op dbid a v])))))
           lookup
           statements)))


(defn ref->v [v]
  (if (map? v) (:db/id v) v))


(defn entity->statements [schema {dbid :db/id :as entity}]   ; entity always has :db/id
  (->> (dissoc entity :db/id)
       (mapcat (fn [[attr val]]
                 (let [cardinality (get-in schema [attr :db/cardinality])
                       valueType (get-in schema [attr :db/valueType])
                       _ (assert cardinality (str "schema attribute not found: " (pr-str attr)))]
                   (if (= valueType :db.type/ref)
                     (condp = cardinality
                       :db.cardinality/one [[:db/add dbid attr (ref->v val)]]
                       :db.cardinality/many (mapv (fn [val] [:db/add dbid attr (ref->v val)]) val))
                     (condp = cardinality
                       :db.cardinality/one [[:db/add dbid attr val]]
                       :db.cardinality/many (mapv (fn [val] [:db/add dbid attr val]) val))))))))


(defn entity? [v]
  (map? v))

(defn entity-children [schema entity]
  (mapcat (fn [[attr val]]
            (let [cardinality (get-in schema [attr :db/cardinality])
                  valueType (get-in schema [attr :db/valueType])]
              (cond
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) [val]
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) (vec val)
                :else [])))
          entity))


(defn pulled-entity->entity [schema dbval {id :db/id :as entity}]
  (with-meta
    (->> (dissoc entity :db/id)                             ; if we add :db/id to the schema this step should not be necessary
         (map (fn [[attr val]]
                [attr (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)
                            _ (assert cardinality (str "schema attribute not found: " (pr-str attr)))]
                        (if (= valueType :db.type/ref)
                          (let [build-DbId #(->DbId (ref->v %) (.-conn-id dbval))]
                            (condp = cardinality
                              :db.cardinality/one (build-DbId val)
                              :db.cardinality/many (set (mapv build-DbId val))))
                          val))]))
         (into {:db/id (->DbId id (.-conn-id dbval))}))
    {:dbval dbval}))


(defn pulled-tree-to-entities [schema dbval pulled-tree]
  (->> (tree-seq (fn [v] (entity? v))
                 #(entity-children schema %)
                 pulled-tree)
       (map #(pulled-entity->entity schema dbval %))
       (reduce (fn [m {dbid :db/id :as entity}]
                 ; order is important on this merge to preserve the meta data
                 (update m dbid #(merge entity %)))
               {})))


(defn pulled-tree-to-statements [schema pulled-tree]
  ;; branch nodes are type entity. which right now is hashmap.
  (let [traversal (tree-seq (fn [v] (entity? v))
                            #(entity-children schema %)
                            pulled-tree)]
    (mapcat #(entity->statements schema %) traversal)))


(defn replace-ids [schema skip? tx]
  (let [id-map (atom {})
        temp-id-atom (atom 0)
        new-temp-id! #(swap! temp-id-atom dec)
        replace-id! (fn [id]
                      (let [new-id (get @id-map id)]
                        (if (nil? new-id)
                          (let [new-id (new-temp-id!)]
                            (swap! id-map assoc id new-id)
                            new-id)
                          new-id)))]
    (mapv (fn [[op e a v]]
            (let [new-e (replace-id! e)
                  valueType (get-in schema [a :db/valueType])
                  new-v (if (and (not (skip? a)) (= valueType :db.type/ref))
                          (replace-id! v)
                          v)]
              [op new-e a new-v]))
          tx)))


(defn recurse-entity->tx [schema graph skip? entity]
  (assert false "todo")
  #_(->> (dissoc entity :db/id)
         (mapcat (fn [[attr val]]
                   (let [cardinality (get-in schema [attr :db/cardinality])
                         valueType (get-in schema [attr :db/valueType])]
                     (if (and (not (skip? attr)) (= valueType :db.type/ref))
                       (condp = cardinality
                         :db.cardinality/one (concat [[:db/add (:db/id entity) attr val]]
                                                     (recurse-entity->tx schema graph skip? (hc/entity graph val)))
                         :db.cardinality/many (mapcat (fn [val]
                                                        (concat [[:db/add (:db/id entity) attr val]]
                                                                (recurse-entity->tx schema graph skip? (hc/entity graph val))))
                                                      val))
                       (condp = cardinality
                         :db.cardinality/one [[:db/add (:db/id entity) attr val]]
                         :db.cardinality/many (mapv (fn [val]
                                                      [:db/add (:db/id entity) attr val])
                                                    val))))))))
