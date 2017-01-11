(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *root-conn-id* nil)


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
  (hydrated? [this named-queries])
  (transact! [this tx]))
