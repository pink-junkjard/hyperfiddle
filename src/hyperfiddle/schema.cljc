(ns hyperfiddle.schema
  (:require
    [cats.monad.exception :as exception]
    [contrib.data :as data]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.Err :as Err]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.legacy :as io-legacy]
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
    (-> (io/hydrate-requests io local-basis staged-branches requests)
        (p/then (fn [{:keys [pulled-trees]}]
                  (->> (map (fn [resultset-or-error request]
                              (if (Err/Err? resultset-or-error)
                                (exception/failure (io-legacy/human-error resultset-or-error request))
                                (exception/success (data/group-by-unique :db/ident resultset-or-error))))
                            pulled-trees
                            requests)
                       (zipmap dbnames)))))))
