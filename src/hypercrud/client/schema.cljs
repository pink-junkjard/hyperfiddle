(ns hypercrud.client.schema
  (:require [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbVal ->QueryRequest]]
            [hypercrud.util :as util]))


(defn schema-request [connection]
  (let [root-dbval (->DbVal hc/*root-conn-id* nil)]
    (->QueryRequest '[:find ?attr :in $ :where [?attr :attribute/ident]]
                    {"$" root-dbval}
                    {"?attr" [root-dbval ['*
                                          {:attribute/valueType [:db/id :db/ident]
                                           :attribute/cardinality [:db/id :db/ident]
                                           :attribute/unique [:db/id :db/ident]
                                           :attribute/hc-type ['*]}]]})))
