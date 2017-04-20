(ns hypercrud.client.core)


(def ^:dynamic *root-conn-id* nil)
(def ^:dynamic *editor-schema* nil)


(defprotocol Peer
  (hydrate [this request])
  (t [this]))


(defprotocol Connection
  (hydrate!* [this requests staged-tx])                     ; internal & used for clone-link
  (hydrate-one! [this request staged-tx])
  (hydrate! [this requests staged-tx force?])
  (hydrated? [this requests])
  (transact! [this tx]))
