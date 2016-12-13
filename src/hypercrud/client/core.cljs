(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)


(defprotocol SuperGraph
  (select [this named-query])
  (get-dbgraph [this dbval])
  (with [this more-statements])
  (t [this]))


(defprotocol DbGraph
  (entity [this dbid])
  (with' [this more-statements]))


(defprotocol Client
  (hydrate! [this named-queries force? staged-tx editor-dbval editor-schema])
  (temp-id! [this conn-id])
  (transact! [this tx]))
