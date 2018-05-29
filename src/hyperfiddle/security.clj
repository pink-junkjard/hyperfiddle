(ns hyperfiddle.security
  (:require [contrib.data :refer [cond-let map-keys]]
            [datomic.api :as d]
            [hypercrud.util.identity :refer [tempid?]]
            [loom.alg :as alg]
            [loom.graph :as graph]))


(def root "hyperfiddle.security/root")

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

(defn write-allow-anonymous [hf-db subject tx]
  tx)

(defn write-authenticated-users-only [hf-db subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    tx))

(defn write-owner-only [hf-db subject tx]
  (if (-> (into #{root} (:hyperfiddle/owners hf-db))
          (contains? subject))
    tx
    (throw (tx-validation-failure))))

(defn entid [db ident]
  (if (tempid? ident)
    ident
    (d/entid db ident)))

(defn inject-tempids-map [db stmt identity-ids component-ids ref-ids]
  (let [top-hash (hash stmt)
        id-count (atom 0)
        inject-tempids-map' (fn inject-tempids-map' [stmt]
                              (let [[e stmt] (if-let [e (:db/id stmt)]
                                               [e stmt]
                                               (let [e (str "hyperfiddle.security." top-hash "." (swap! id-count inc))]
                                                 [e (assoc stmt :db/id e)]))]
                                (reduce (fn [acc-map [a v :as kv]]
                                          (let [a (or (entid db a) a)
                                                [{:keys [identity-graph component-edges statement]} v]
                                                (if (and (map? v) (contains? ref-ids a))
                                                  (let [child-map (inject-tempids-map' v)]
                                                    [{:identity-graph (graph/graph (:identity-graph acc-map) (:identity-graph child-map))
                                                      :component-edges (into (:component-edges acc-map) (:component-edges child-map))
                                                      :statement (:statement acc-map)}
                                                     (:statement child-map)])
                                                  [acc-map v])]
                                            {:identity-graph (if (contains? identity-ids a)
                                                               (graph/add-edges identity-graph [e [a v]])
                                                               identity-graph)
                                             :component-edges (if (contains? component-ids a)
                                                                (conj component-edges [e (:db/id v)])
                                                                component-edges)
                                             :statement (assoc statement (first kv) v)}))
                                        {:identity-graph (graph/graph e)
                                         :component-edges #{}
                                         :statement {}}
                                        stmt)))]
    (inject-tempids-map' stmt)))

(defn inject-tempids [db tx]
  (let [identity-ids (into #{} (d/q '[:find [?e ...]
                                      :where
                                      [?e :db/ident]
                                      [?e :db/unique :db.unique/identity]]
                                    db))
        component-ids (into #{} (d/q '[:find [?e ...]
                                       :where
                                       [?e :db/ident]
                                       [?e :db/isComponent true]]
                                     db))
        ref-ids (into #{} (d/q '[:find [?e ...]
                                 :where
                                 [?e :db/ident]
                                 [?e :db/valueType :db.type/ref]]
                               db))
        acc (reduce (fn [{:keys [component-edges identity-graph tx]} stmt]
                      (cond
                        (map? stmt) (let [sub (inject-tempids-map db stmt identity-ids component-ids ref-ids)]
                                      {:identity-graph (graph/graph identity-graph (:identity-graph sub))
                                       :component-edges (into component-edges (:component-edges sub))
                                       :tx (conj tx (:statement sub))})

                        (or (list? stmt) (vector? stmt))
                        (-> (let [op (:db/ident (d/pull db [:db/ident] (first stmt)))]
                              (condp contains? op
                                #{:db/add :db/retract}
                                (let [[_ e a v] stmt
                                      a (or (entid db a) a)]
                                  {:identity-graph (if (contains? identity-ids a)
                                                     (graph/add-edges identity-graph [e [a v]])
                                                     (graph/add-nodes identity-graph e))
                                   :component-edges (if (contains? component-ids a)
                                                      (conj component-edges [e v])
                                                      component-edges)})

                                #{:db/retractEntity :db.fn/retractEntity}
                                {:identity-graph (graph/add-nodes identity-graph (second stmt))
                                 :component-edges component-edges}

                                #{:db/cas :db.fn/cas}
                                (let [[_ e a ov nv] stmt
                                      a (or (entid db a) a)]
                                  {:identity-graph (if (contains? identity-ids a)
                                                     (graph/add-edges identity-graph [e [a ov]] [e [a nv]])
                                                     (graph/add-nodes identity-graph e))
                                   :component-edges (if (contains? component-ids a)
                                                      (conj component-edges [e nv] [e ov])
                                                      component-edges)})))
                            (assoc :tx (conj tx stmt)))))
                    {:identity-graph (graph/graph)
                     :component-edges #{}
                     :tx []}
                    tx)]
    {:component-graph (let [ig (:identity-graph acc)]
                        (->> (:component-edges acc)
                             (map (fn [[parent child]]
                                    [(set (alg/bf-traverse ig parent)) (set (alg/bf-traverse ig child))]))
                             (apply graph/digraph)))
     :identity-graph (:identity-graph acc)
     :tx-with-tempids (:tx acc)}))

(defn ->existing-eids [db eids]
  (->> eids
       (remove tempid?)
       (map (partial d/entid db))
       (remove nil?)                                        ; nil will be from new lookup refs
       (into #{})))

(defn validate-write-entity-ownership [config db tx]
  (let [supported-ops #{:db/add :db/retract :db/retractEntity :db.fn/retractEntity :db/cas :db.fn/cas}]
    (-> (->> tx
             (remove map?)
             (every? #(contains? supported-ops (first %))))
        (assert "user declared db functions not yet supported")))

  (let [{:keys [component-graph identity-graph tx-with-tempids]} (inject-tempids db tx)]
    ; check permissions on existing entities
    (doseq [existing-eids (->> (alg/connected-components component-graph)
                               (mapcat (fn [list-of-sets-of-eids]
                                         ; only need one eid per identity
                                         (let [x (map first list-of-sets-of-eids)]
                                           (map vector x (drop 1 x)))))
                               (apply graph/add-edges identity-graph)
                               (alg/connected-components)
                               (map (partial ->existing-eids db)))
            :when (not (empty? existing-eids))]
      (when-not (apply (:can-merge? config) existing-eids)
        (throw (tx-validation-failure))))

    (->> (alg/connected-components identity-graph)
         (mapcat (fn [eids]
                   (let [existing-eids (->existing-eids db eids)]
                     (when (and (empty? existing-eids)      ; only generate owner if ALL merged entities are new
                                (= 0 (graph/in-degree component-graph (set eids)))) ; this entity is not a component of another new entity
                       ; doesn't matter which tempid has owner appended
                       (let [tempid (some #(when (tempid? %) %) eids)]
                         (assert tempid "no tempids or existing ids") ; this is probably a bad transaction; would fail with "Unable to resolve entity"
                         ((:generate-owner config) tempid))))))
         (into tx-with-tempids))))

(defn build-parent-lookup [db]
  (let [component-ids (into #{} (d/q '[:find [?e ...]
                                       :where
                                       [?e :db/ident]
                                       [?e :db/isComponent true]]
                                     db))]
    (fn [e]
      (letfn [(component-parents [e]
                (let [parent-eids (->> (d/datoms db :vaet e)
                                       (filter #(contains? component-ids (.a %)))
                                       (map #(.e %)))]
                  ; todo concat untransacted tempids
                  ; todo solve infinite loops
                  (->> parent-eids
                       (mapcat (fn [eid]
                                 (let [parent-eids (component-parents eid)]
                                   (if (empty? parent-eids)
                                     [eid]
                                     parent-eids))))
                       (into #{}))))]
        (let [top-level-eids (component-parents e)]
          (assert (> 2 (count top-level-eids)) "Multiple parents found for component entity")
          (first top-level-eids))))))

(defn write-domains [hf-db subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    (let [db (d/db (d/connect (str (:database/uri hf-db))))
          parent-lookup (build-parent-lookup db)
          owners (fn [eid]
                   (let [eid (or (parent-lookup eid) eid)
                         e (d/entity db eid)]
                     (into #{} (:hyperfiddle/owners e))))
          has-permissions? (if (= root subject)
                             (constantly true)
                             #(contains? % subject))
          config {:can-merge? (fn [eid & rest]
                                (let [first-owners (owners eid)]
                                  ; unless the perms are identical, the entities cannot be merged
                                  (and (has-permissions? first-owners)
                                       (every? #(= first-owners (owners %)) rest))))
                  :generate-owner (fn [e] [[:db/add e :hyperfiddle/owners subject]])}]
      (validate-write-entity-ownership config db tx))))

#_(defn write-entity-ownership [hf-db subject tx]
    (build-write-entity-ownership
      {:can-merge? (fn [& es]
                     (->> es
                          (map :hyperfiddle/owners)
                          (into #{})
                          (count)
                          (= 1)))
       :can-write? (fn [hf-db db subject e]
                     (and (not (nil? subject))
                          (or (nil? e)                      ; entity is new
                              (= subject (:hypercrud.security/owner e)))))
       :generate-owner (fn [hf-db db subject e] [[:db/add e :hyperfiddle/owners subject]])}))
