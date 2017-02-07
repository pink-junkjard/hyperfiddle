(ns hypercrud.client.core)


(def ^:dynamic *temp-id!*)
(def ^:dynamic *root-conn-id* nil)
(def ^:dynamic *editor-schema* nil)


(defprotocol Peer
  (hydrate [this request])
  (t [this]))


(defprotocol Connection
  (hydrate! [this requests staged-tx force?])
  (hydrated? [this requests])
  (transact! [this tx]))
