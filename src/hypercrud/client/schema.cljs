(ns hypercrud.client.schema
  (:require [hypercrud.types :refer [->QueryRequest]]))


(defn schema-request [root-db connection]
  (->QueryRequest '[:find ?attr :in $ :where [?attr :attribute/ident]]
                  {"$" root-db}
                  {"?attr" [root-db ['*
                                     {:attribute/valueType [:db/id :db/ident]
                                      :attribute/cardinality [:db/id :db/ident]
                                      :attribute/unique [:db/id :db/ident]
                                      :attribute/hc-type ['*]}]]}))
