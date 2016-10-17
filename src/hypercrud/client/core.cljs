(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *user-profile*)


(defprotocol Graph
  (select [this named-query]
          [this named-query query-message])
  (entity [this eid])
  (with [this more-statements])
  (t [this]))


(defprotocol Client
  (graph [this])
  (hydrate! [this named-queries t])
  (temp-id! [this])
  (transact! [this tx]))
