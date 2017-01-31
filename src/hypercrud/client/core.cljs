(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *root-conn-id* nil)


(defprotocol SuperGraph
  (select [this query-request])
  (entity [this entity-request])
  (request [this request])
  (with [this more-statements])
  (t [this]))


(defprotocol Client
  (hydrate! [this request force? staged-tx editor-dbval editor-schema])
  (hydrated? [this request])
  (transact! [this tx]))
