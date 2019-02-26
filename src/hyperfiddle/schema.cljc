(ns hyperfiddle.schema
  (:require
    [contrib.data :as data]
    [contrib.datomic]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.io.hydrate-requests :as hydrate-requests]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]))


(defn request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :where [:db.part/db :db.install/attribute ?attr]]
                  [dbval]))

(defn hydrate-schemas-for-uris [rt branch uris]
  (let [uris (vec (distinct uris))
        local-basis @(runtime/state rt [::runtime/partitions branch :local-basis])
        stage nil
        requests (map (fn [uri] (request (hc/db rt uri branch))) uris)]
    (-> (hydrate-requests/hydrate-all-or-nothing! rt local-basis stage requests)
        (p/then (fn [schemas]
                  (->> (map contrib.datomic/indexed-schema schemas)
                       (zipmap uris)))))))

(defn hydrate-schemas [rt branch]
  (let [uris @(r/fmap->> (runtime/state rt [::runtime/domain :domain/databases])
                         (map (fn [hf-db] (get-in hf-db [:domain.database/record :database/uri]))))]
    (hydrate-schemas-for-uris rt branch uris)))
