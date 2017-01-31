(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *root-conn-id* nil)


(defprotocol Peer
  (hydrate [this request])
  (t [this]))


(defprotocol Connection
  (hydrate! [this requests staged-tx force? editor-dbval editor-schema])
  (hydrated? [this requests])
  (transact! [this tx]))
