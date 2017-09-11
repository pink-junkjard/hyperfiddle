(ns hypercrud.client.schema
  (:require [cats.core :as cats]
            [hypercrud.client.core :as hc]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.core :as util]))

(defn fe->db [fe param-ctx]
  (let [fe-conn (:find-element/connection fe)]
    (let [conn-id (-> fe-conn :db/id :id)
          branch (:branch param-ctx)]
      (hc/db (:peer param-ctx) conn-id branch))))

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

(defn schema-requests-for-link [ordered-fes param-ctx]
  (->> ordered-fes
       (map #(fe->db % param-ctx))
       (map schema-request)
       (concat [(hc-attr-request param-ctx)])))

(defn hydrate-schema [ordered-fes param-ctx]
  (-> (hc/hydrate (:peer param-ctx) (hc-attr-request param-ctx))
      (cats/bind
        (fn [root-data]
          (let [indexed-root (->> root-data
                                  (mapv #(get % "?attr"))
                                  (util/group-by-assume-unique :attribute/ident)
                                  (util/map-values #(dissoc % :attribute/ident :db/id)))]
            (->> ordered-fes
                 (mapv (fn [fe]
                         (->> (fe->db fe param-ctx)
                              (schema-request)
                              (hc/hydrate (:peer param-ctx))
                              (cats/fmap (fn [schema]
                                           [(:find-element/name fe)
                                            (->> schema
                                                 (mapv #(get % "?attr"))
                                                 (util/group-by-assume-unique :db/ident)
                                                 (merge-with merge indexed-root))])))))
                 (cats/sequence)
                 (cats/fmap #(into {} %))))))))
