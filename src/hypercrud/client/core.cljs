(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *user-profile*)


(defprotocol SuperGraph
  (select [this named-query]
          [this named-query query-message])
  (entity [this dbval dbid])
  (with [this dbval more-statements])
  (t [this]))


(defprotocol DbGraph
  (entity' [this eid])
  (with' [this more-statements]))


(defprotocol Client
  (graphs [this])
  (hydrate! [this named-queries])
  (temp-id! [this conn-id])
  (transact! [this tx]))
