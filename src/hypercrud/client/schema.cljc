(ns hypercrud.client.schema
  (:require
    [cats.core :as cats]
    [contrib.data :refer [group-by-assume-unique map-values]]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]))


(defn hc-attr-request [ctx]
  (->QueryRequest '[:find [(pull ?attr [:attribute/ident :attribute/renderer :db/doc]) ...]
                    :where [?attr :attribute/ident]]
                  [(hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/domain :domain/fiddle-database :database/uri]) (:branch ctx))]))

(defn schema-request [dbval]
  (->QueryRequest '[:in $ :find [(pull ?attr [*
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
                    schema-request))))
       (concat [(hc-attr-request ctx)])))

(letfn [(with-root-data [ctx either-root-data]
          (cats/bind either-root-data
                     (fn [root-data]
                       (let [indexed-root (->> root-data
                                               (map #(into {} %))
                                               (group-by-assume-unique :attribute/ident)
                                               (map-values #(dissoc % :attribute/ident :db/id)))]

                         (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
                              (mapv (fn [hf-db]
                                      (let [uri (get-in hf-db [:domain.database/record :database/uri])
                                            request (schema-request (hc/db (:peer ctx) uri (:branch ctx)))]
                                        (->> @(hc/hydrate (:peer ctx) (:branch ctx) request)
                                             (cats/fmap (fn [schema]
                                                          [(:domain.database/name hf-db)
                                                           (->> schema
                                                                (map #(into {} %))
                                                                (group-by-assume-unique :db/ident)
                                                                (merge-with #(merge %2 %1) indexed-root))]))))))
                              (cats/sequence)
                              (cats/fmap #(into {} %)))))))]
  (defn hydrate-schema [ctx]
    (->> (hc/hydrate (:peer ctx) (:branch ctx) (hc-attr-request ctx))
         (r/fmap (r/partial with-root-data ctx)))))
