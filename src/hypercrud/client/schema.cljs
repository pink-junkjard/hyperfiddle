(ns hypercrud.client.schema
  (:require [cats.core :as cats]
            [hypercrud.client.core :as hc]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.core :as util]))


(defn hc-attr-request [param-ctx]
  (let [dbval (hc/db (:peer param-ctx) hc/*root-conn-id* (:branch param-ctx))]
    (->QueryRequest '[:find ?attr :in $ :where
                      (or [?attr :attribute/renderer]
                          [?attr :attribute/hc-type])]
                    {"$" dbval}
                    {"?attr" [dbval [:attribute/ident
                                     :attribute/renderer
                                     {:attribute/hc-type ['*]}]]})))

(defn schema-request [dbval]
  (->QueryRequest '[:find ?attr :in $ :where [:db.part/db :db.install/attribute ?attr]]
                  {"$" dbval}
                  {"?attr" [dbval ['*
                                   {:db/valueType [:db/id :db/ident]
                                    :db/cardinality [:db/id :db/ident]
                                    :db/unique [:db/id :db/ident]}]]}))

(defn schema-requests-for-link [ordered-fes ctx]
  (->> ordered-fes
       (map (fn [fe]
              (->> (hc/db (:peer ctx) (get-in ctx [:domain-dbs (:find-element/connection fe)]) (:branch ctx))
                   (schema-request))))
       (concat [(hc-attr-request ctx)])))

(defn hydrate-schema [ordered-fes ctx]
  (-> (hc/hydrate (:peer ctx) (hc-attr-request ctx))
      (cats/bind
        (fn [root-data]
          (let [indexed-root (->> root-data
                                  (mapv #(get % "?attr"))
                                  (util/group-by-assume-unique :attribute/ident)
                                  (util/map-values #(dissoc % :attribute/ident :db/id)))]
            (->> ordered-fes
                 (mapv (fn [fe]
                         (->> (hc/db (:peer ctx) (get-in ctx [:domain-dbs (:find-element/connection fe)]) (:branch ctx))
                              (schema-request)
                              (hc/hydrate (:peer ctx))
                              (cats/fmap (fn [schema]
                                           [(:find-element/name fe)
                                            (->> schema
                                                 (mapv #(get % "?attr"))
                                                 (util/group-by-assume-unique :db/ident)
                                                 (merge-with merge indexed-root))])))))
                 (cats/sequence)
                 (cats/fmap #(into {} %))))))))
