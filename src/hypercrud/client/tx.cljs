(ns hypercrud.client.tx
  (:require [cljs.core.match :refer-macros [match]]
            [clojure.set :as set]
            [hypercrud.types :refer [->DbId ->Entity] :as types]))


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
                                :db.cardinality/one {:old (let [old-val (some-> old-val .-dbid)]
                                                            (if (nil? old-val) [] [old-val]))
                                                     :new [new-val]}
                                :db.cardinality/many {:old (map #(.-dbid %) old-val)
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
  (let [attribute (get (-> entity .-dbgraph .-schema) a)]
    (->Entity (.-dbgraph entity) (.-dbid entity) (apply-stmt-to-data attribute (.-data entity) [op e a v]) {})))


(defn get-data! [lookup-transient dbid]
  (if-let [entity-data (get lookup-transient dbid)]
    [lookup-transient entity-data]
    (let [entity-data {:db/id dbid}]
      [(assoc! lookup-transient dbid entity-data) entity-data])))


(defn apply-stmt-to-entity-data! [schema lookup-transient entity-data [op dbid a v]]
  (let [attribute (get schema a)
        lookup-transient (condp = (:db/valueType attribute)
                           :db.type/ref (first (get-data! lookup-transient v))
                           lookup-transient)
        data (apply-stmt-to-data attribute entity-data [op dbid a v])]
    (assoc! lookup-transient dbid data)))


(defn build-data-lookup
  ([schema statements] (build-data-lookup schema statements {}))
  ([schema statements lookup]
   (->> statements
        (reduce (fn [lookup-transient [op dbid a v]]
                  (let [[lookup-transient entity-data] (get-data! lookup-transient dbid)]
                    (apply-stmt-to-entity-data! schema lookup-transient entity-data [op dbid a v])))
                (transient lookup))
        persistent!)))


(defn ref->v [v]
  (cond (map? v) (:db/id v)
        (instance? types/Entity v) (.-dbid v)
        :else v))


(defn entity->statements [schema {dbid :db/id :as entity}]  ; entity always has :db/id
  ;; dissoc :db/id, it is done as a seq/filter for compatibility with #Entity
  (->> (seq entity)
       (filter (fn [[k v]] (not= :db/id k)))

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


; applies an attribute and value pair to an entity-data-map
; THIS DOES NOT ASSOC the new entity-data-map into the lookup-transient (would be unncessary thrash)
(defn- apply-av-to-entity-map [schema conn-id tempids [lookup-transient entity-data] [attr val]]
  (let [{:keys [:db/cardinality :db/valueType]} (get schema attr)
        _ (assert cardinality (str "schema attribute not found: " (pr-str attr)))
        build-DbId! (fn [lookup-transient ref]
                      (let [id (ref->v ref)
                            id (or (get tempids id) id)     ; if a tempid was sent up on the wire we need to convert it back now for value position
                            dbid (->DbId id conn-id)
                            [lookup-transient _] (get-data! lookup-transient dbid)]
                        [lookup-transient dbid]))]
    (if (= valueType :db.type/ref)
      (condp = cardinality
        :db.cardinality/one (let [[lookup-transient dbid] (build-DbId! lookup-transient val)]
                              [lookup-transient (assoc entity-data attr dbid)])
        :db.cardinality/many (let [[lookup-transient dbids] (reduce (fn [[lookup-transient dbids] v]
                                                                      (let [[lookup-transient dbid] (build-DbId! lookup-transient v)]
                                                                        [lookup-transient (conj dbids dbid)]))
                                                                    [lookup-transient #{}]
                                                                    val)]
                               [lookup-transient (update entity-data attr (fnil #(set/union % dbids) #{}))]))
      (let [new-entity-data (condp = cardinality
                              :db.cardinality/one (assoc entity-data attr val)
                              :db.cardinality/many (update entity-data attr (fnil #(set/union % (set val)) #{})))]
        ; non-ref types don't manipulate the lookup-transient at all
        [lookup-transient new-entity-data]))))


(defn process-entity-data-map! [schema conn-id tempids lookup-transient entity-map]
  (let [id (:db/id entity-map)
        id (or (get tempids id) id)     ; if a tempid was sent up on the wire we need to convert it back now for entity position
        dbid (->DbId id conn-id)
        [lookup-transient new-entity-map] (reduce (partial apply-av-to-entity-map schema conn-id tempids)
                                                  (get-data! lookup-transient dbid)
                                                  (dissoc entity-map :db/id))]
    (assoc! lookup-transient dbid new-entity-map)))


(defn pulled-tree-to-data-lookup [schema conn-id pulled-trees tempids]
  (->> pulled-trees
       (reduce (fn [lookup-transient pulled-tree]
                 (->> (tree-seq (fn [v] (entity? v))
                                #(entity-children schema %)
                                pulled-tree)
                      (reduce (partial process-entity-data-map! schema conn-id tempids)
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
