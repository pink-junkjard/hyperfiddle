(ns hyperfiddle.schema
  (:require
    [contrib.data :as data]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]))


(defn request [dbval]
  (->QueryRequest '[:find [(pull ?attr [*
                                        {:db/valueType [:db/ident]
                                         :db/cardinality [:db/ident]
                                         :db/unique [:db/ident]}]) ...]
                    :where [:db.part/db :db.install/attribute ?attr]]
                  [dbval]))

(defn hydrate-schemas [rt branch]
  (let [local-basis @(runtime/state rt [::runtime/partitions branch :local-basis])
        staged-branches nil
        dbnames (-> (runtime/domain rt)
                    domain/databases
                    keys
                    vec)
        requests (mapv (fn [dbname] (request (->DbRef dbname branch))) dbnames)]
    (-> (io/hydrate-all-or-nothing! (runtime/io rt) local-basis staged-branches requests)
        (p/then (fn [schemas]
                  (->> (map #(data/group-by-unique :db/ident %) schemas)
                       (zipmap dbnames)))))))
