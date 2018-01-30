(ns hypercrud.client.schema
  (:require [cats.core :as cats]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.URI :refer [#?(:cljs URI)]]
            [hypercrud.util.core :as util])
  #?(:clj
     (:import (java.net URI))))


(defn hc-attr-request [ctx]
  (->QueryRequest '[:find [(pull ?attr [:attribute/ident :attribute/renderer :db/doc]) ...]
                    :where [?attr :attribute/ident]]
                  [(hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/repository :dbhole/uri] ctx) (:branch ctx))]))

(defn schema-request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :where [:db.part/db :db.install/attribute ?attr]]
                  [dbval]))

(defn schema-requests-for-link [ctx]
  (->> (get-in ctx [:hypercrud.browser/repository :repository/environment])
       (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
       (map (fn [[dbname uri]]
              (->> (hc/db (:peer ctx) uri (:branch ctx))
                   (schema-request))))
       (concat [(hc-attr-request ctx)])))

(defn hydrate-schema [ctx]
  (-> @(hc/hydrate (:peer ctx) (hc-attr-request ctx))
      (cats/bind
        (fn [root-data]
          (let [indexed-root (->> root-data
                                  (map #(into {} %))
                                  (util/group-by-assume-unique :attribute/ident)
                                  (util/map-values #(dissoc % :attribute/ident :db/id)))]

            (->> (get-in ctx [:hypercrud.browser/repository :repository/environment])
                 (filter (fn [[k v]] (and (string? k) (string/starts-with? k "$") (instance? URI v))))
                 (mapv (fn [[dbname uri]]
                         (let [request (schema-request (hc/db (:peer ctx) uri (:branch ctx)))]
                           (->> @(hc/hydrate (:peer ctx) request)
                                (cats/fmap (fn [schema]
                                             [dbname
                                              (->> schema
                                                   (map #(into {} %))
                                                   (util/group-by-assume-unique :db/ident)
                                                   (merge-with #(merge %2 %1) indexed-root))]))))))
                 (cats/sequence)
                 (cats/fmap #(into {} %))))))))
