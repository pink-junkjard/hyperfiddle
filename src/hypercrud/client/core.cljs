(ns hypercrud.client.core)


(defprotocol Graph
  (select [this named-query])
  (entity [this eid])
  (with [this more-statements])
  (t [this]))


(defprotocol Client
  (graph [this])
  (hydrate! [this named-queries t])
  (transact! [this tx]))


(defn tempid!-factory []
  (let [n (atom 0)]
    (fn [] (swap! n dec) @n)))
