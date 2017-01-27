(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *root-conn-id* nil)


(defprotocol SuperGraph
  (select [this query-request])
  (get-dbgraph [this dbval])
  (with [this more-statements])
  (t [this]))


(defprotocol DbGraph
  (entity [this dbid])
  (with' [this more-statements]))


(defprotocol Client
  (hydrate! [this request force? staged-tx editor-dbval editor-schema])
  (hydrated? [this request])
  (transact! [this tx]))
