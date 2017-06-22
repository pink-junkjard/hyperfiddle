(ns hypercrud.client.core)


(def ^:dynamic *root-conn-id* nil)
(def ^:dynamic *peer* nil)

(defprotocol Peer
  (hydrate [this request])
  (db [this conn-id branch])

  ; used for clone-link
  (hydrate-one! [this request]))
