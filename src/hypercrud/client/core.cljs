(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)


(defprotocol SuperGraph
  (select [this named-query]
          [this named-query query-message])
  (get-dbgraph [this dbval])
  (with [this more-statements])
  (t [this]))


(defprotocol DbGraph
  (entity [this dbid])
  (with' [this more-statements]))


(defprotocol Client
  (hydrate! [this schemas named-queries force? staged-tx])
  (temp-id! [this conn-id])
  (transact! [this tx]))
