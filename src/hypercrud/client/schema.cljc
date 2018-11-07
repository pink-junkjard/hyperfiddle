(ns hypercrud.client.schema                                 ; legacy to be removed
  (:require
    [cats.core :as cats]
    [contrib.data :as data]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


(defn schema-request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :where
                    [:db.part/db :db.install/attribute ?attr]
                    [(hyperfiddle.query/attr-not-archived? $ ?attr)]]
                  [dbval]))

(defn schema-requests-for-link [ctx]
  (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
       (map (fn [hf-db]
              (let [uri (get-in hf-db [:domain.database/record :database/uri])]
                (-> (hc/db (:peer ctx) uri (:branch ctx))
                    schema-request))))))

(defn hydrate-schema [ctx]
  (let [dbnames (map :domain.database/name (get-in ctx [:hypercrud.browser/domain :domain/databases]))]
    (->> (schema-requests-for-link ctx)
         (mapv (fn [request]
                 @(r/apply-inner-r
                    (r/fmap->> (hc/hydrate (:peer ctx) (:branch ctx) request)
                               (cats/fmap (r/partial data/group-by-unique :db/ident))))))
         (cats/sequence)
         (cats/fmap (partial zipmap dbnames)))))
