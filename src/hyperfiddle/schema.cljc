(ns hyperfiddle.schema
  (:require
    [contrib.data :as data]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [promesa.core :as p]))


(defn request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :where [:db.part/db :db.install/attribute ?attr]]
                  [dbval]))

(defn hydrate-schemas [io domain local-basis branch staged-branches]
  (let [dbnames (-> (domain/databases domain)
                    keys
                    vec)
        requests (mapv (fn [dbname] (request (->DbRef dbname branch))) dbnames)]
    (-> (io/hydrate-all-or-nothing! io local-basis staged-branches requests)
        (p/then (fn [schemas]
                  (->> (map #(data/group-by-unique :db/ident %) schemas)
                       (zipmap dbnames)))))))
