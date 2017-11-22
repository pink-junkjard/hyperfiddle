(ns hypercrud.client.schema
  (:require [cats.core :as cats]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.URI :refer [URI]]
            [hypercrud.util.core :as util]))


(defn hc-attr-request [ctx]
  (->QueryRequest '[:find [(pull ?attr [:attribute/ident :attribute/renderer]) ...]
                    :in $ :where
                    [?attr :attribute/renderer]]
                  {"$" (hc/db (:peer ctx) (get-in ctx [:repository :dbhole/uri] ctx) (:branch ctx))}
                  nil))

(defn schema-request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :in $ :where [:db.part/db :db.install/attribute ?attr]]
                  {"$" dbval}
                  nil))

(defn schema-requests-for-link [ctx]
  (->> (get-in ctx [:repository :repository/environment])
       (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
       (map (fn [[dbname uri]]
              (->> (hc/db (:peer ctx) uri (:branch ctx))
                   (schema-request))))
       (concat [(hc-attr-request ctx)])))

(defn hydrate-schema [ctx]
  (-> (hc/hydrate (:peer ctx) (hc-attr-request ctx))
      (cats/bind
        (fn [root-data]
          (let [indexed-root (->> root-data
                                  (map #(into {} %))
                                  (util/group-by-assume-unique :attribute/ident)
                                  (util/map-values #(dissoc % :attribute/ident :db/id)))]

            (->> (get-in ctx [:repository :repository/environment])
                 (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
                 (mapv (fn [[dbname uri]]
                         (->> (hc/db (:peer ctx) uri (:branch ctx))
                              (schema-request)
                              (hc/hydrate (:peer ctx))
                              (cats/fmap (fn [schema]
                                           [dbname
                                            (->> schema
                                                 (map #(into {} %))
                                                 (util/group-by-assume-unique :db/ident)
                                                 (merge-with merge indexed-root))])))))
                 (cats/sequence)
                 (cats/fmap #(into {} %))))))))
