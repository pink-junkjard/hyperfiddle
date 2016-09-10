(ns hypercrud.client.core)


(defprotocol Graph
  (select [this named-query]
          [this named-query query-message])
  (entity [this eid])
  (with [this more-statements])
  (t [this])
  (temp-id! [this]))


(defprotocol Client
  (graph [this])
  (hydrate! [this named-queries t])
  (transact! [this tx]))
