(ns hypercrud.client.tx
  (:require [cljs.core.match :refer-macros [match]]
            [hypercrud.types :refer [->DbId ->Entity]]))


(defn tempid? [dbid] (< (.-id dbid) 0))


(defn edit-entity [dbid a rets adds]
  (vec (concat (map (fn [val] [:db/retract dbid a val]) rets)
               (map (fn [val] [:db/add dbid a val]) adds))))


(defn update-entity-attr [{:keys [:db/id] :as entity}
                          {:keys [:attribute/ident :attribute/cardinality :attribute/valueType] :as attribute}
                          new-val]
  (let [{:keys [old new]} (let [old-val (get entity ident)]
                            (if (= (:db/ident valueType) :db.type/ref)
                              (condp = (:db/ident cardinality)
                                :db.cardinality/one {:old [(some-> old-val .-dbid)]
                                                     :new [new-val]}
                                :db.cardinality/many {:old (map #(.-dbid %) old-val)
                                                      :new new-val})
                              (condp = (:db/ident cardinality)
                                :db.cardinality/one {:old [old-val]
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


(defn get-stable-entity-reference! [dbgraph lookup-transient dbid]
  (if-let [entity (get lookup-transient dbid)]
    [lookup-transient entity]
    (let [entity (->Entity dbgraph dbid {:db/id dbid})]
      [(assoc! lookup-transient dbid entity) entity])))


; v needs to be preprocessed into an entity already
(defn apply-stmt-to-data [attribute data [op _ a v]]
  (let [cardinality (:db/cardinality attribute)
        _ (assert cardinality (str "schema attribute not found: " (pr-str a)))]
    (cond
      (and (= op :db/add) (= cardinality :db.cardinality/one)) (assoc data a v)
      (and (= op :db/retract) (= cardinality :db.cardinality/one)) (dissoc data a)
      (and (= op :db/add) (= cardinality :db.cardinality/many)) (update-in data [a] (fnil #(conj % v) #{}))
      (and (= op :db/retract) (= cardinality :db.cardinality/many)) (update-in data [a] (fnil #(disj % v) #{}))
      :else (throw "match error"))))


(defn merge-entity-and-stmt [entity [op e a v]]
  (let [attribute (get (-> entity .-dbgraph .-schema) a)
        v (condp = (:db/valueType attribute)
            :db.type/ref (let [ent-v (get (-> entity .-dbgraph .-entity-lookup) v)]
                           (assert (not= nil ent-v) (str "Unable to find entity " (pr-str v)))
                           ent-v)
            v)]
    (->Entity (.-dbgraph entity) (.-dbid entity) (apply-stmt-to-data attribute (.-data entity) [op e a v]))))


(defn apply-stmt-to-entity! [dbgraph lookup-transient entity [op e a v]]
  (let [attribute (get (.-schema dbgraph) a)
        [lookup-transient v] (condp = (:db/valueType attribute)
                               :db.type/ref (get-stable-entity-reference! dbgraph lookup-transient v)
                               [lookup-transient v])
        data (apply-stmt-to-data attribute (.-data entity) [op e a v])]
    (aset entity "data" data)
    lookup-transient))


(defn build-entity-lookup
  ([dbgraph statements] (build-entity-lookup dbgraph statements {}))
  ([dbgraph statements lookup]
   (->> statements
        (reduce (fn [lookup-transient [op dbid a v]]
                  (let [[lookup-transient entity] (get-stable-entity-reference! dbgraph lookup-transient dbid)]
                    (apply-stmt-to-entity! dbgraph lookup-transient entity [op dbid a v])))
                (transient lookup))
        persistent!)))


(defn ref->v [v]
  (if (map? v) (:db/id v) v))


(defn entity->statements [schema {dbid :db/id :as entity}]  ; entity always has :db/id
  (->> (dissoc entity :db/id)
       (mapcat (fn [[attr val]]
                 (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)
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
            (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
              (cond
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) [val]
                (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) (vec val)
                :else [])))
          entity))


(defn process-entity-map! [dbgraph conn-id lookup-transient entity-map]
  (let [dbid (->DbId (:db/id entity-map) conn-id)
        [lookup-transient entity] (get-stable-entity-reference! dbgraph lookup-transient dbid)]
    (reduce (fn [lookup-transient [attr val]]
              (let [{:keys [:db/cardinality :db/valueType]} (get (.-schema dbgraph) attr)
                    _ (assert cardinality (str "schema attribute not found: " (pr-str attr)))
                    update-entity! (fn [lookup-transient v]
                                     (apply-stmt-to-entity! dbgraph lookup-transient entity [:db/add dbid attr v]))
                    build-DbId #(->DbId (ref->v %) conn-id)]
                (if (= valueType :db.type/ref)
                  (condp = cardinality
                    :db.cardinality/one (update-entity! lookup-transient (build-DbId val))
                    :db.cardinality/many (reduce #(update-entity! %1 (build-DbId %2))
                                                 lookup-transient
                                                 val))
                  (condp = cardinality
                    :db.cardinality/one (update-entity! lookup-transient val)
                    :db.cardinality/many (reduce #(update-entity! %1 %2)
                                                 lookup-transient
                                                 val)))))
            lookup-transient
            (dissoc entity-map :db/id))))


(defn pulled-tree-to-entity-lookup [dbgraph conn-id pulled-trees]
  (->> pulled-trees
       (reduce (fn [lookup-transient pulled-tree]
                 (->> (tree-seq (fn [v] (entity? v))
                                #(entity-children (.-schema dbgraph) %)
                                pulled-tree)
                      (reduce (partial process-entity-map! dbgraph conn-id)
                              lookup-transient)))
               (transient {}))
       persistent!))


(defn pulled-tree-to-statements [schema pulled-tree]
  ;; branch nodes are type entity. which right now is hashmap.
  (let [traversal (tree-seq (fn [v] (entity? v))
                            #(entity-children schema %)
                            pulled-tree)]
    (mapcat #(entity->statements schema %) traversal)))


(defn replace-ids [schema conn-id skip? tx]
  (let [id-map (atom {})
        temp-id-atom (atom 0)
        new-temp-id! #(swap! temp-id-atom dec)
        replace-id! (fn [dbid]
                      (let [new-dbid (get @id-map dbid)]
                        (if (nil? new-dbid)
                          (let [new-dbid (->DbId (new-temp-id!) conn-id)]
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


(defn recurse-entity->tx [schema graph skip? entity]
  (->> (dissoc (.-data entity) :db/id)
       (mapcat (fn [[attr val]]
                 (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)]
                   (if (= valueType :db.type/ref)
                     (match [cardinality (skip? attr)]
                            [:db.cardinality/one true] [[:db/add (:db/id entity) attr (.-dbid val)]]
                            [:db.cardinality/one false] (concat [[:db/add (:db/id entity) attr (.-dbid val)]]
                                                                (recurse-entity->tx schema graph skip? val))
                            [:db.cardinality/many true] (mapv (fn [val]
                                                                [:db/add (:db/id entity) attr (.-dbid val)])
                                                              val)
                            [:db.cardinality/many false] (mapcat (fn [val]
                                                                   (concat [[:db/add (:db/id entity) attr (.-dbid val)]]
                                                                           (recurse-entity->tx schema graph skip? val)))
                                                                 val))
                     (condp = cardinality
                       :db.cardinality/one [[:db/add (:db/id entity) attr val]]
                       :db.cardinality/many (mapv (fn [val]
                                                    [:db/add (:db/id entity) attr val])
                                                  val))))))))
