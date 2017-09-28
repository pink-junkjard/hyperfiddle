(ns hypercrud.client.core)


; todo rename and move me, I'm actually a uri, and belong in the browser ns
(def ^:dynamic *root-conn-id* nil)

(defprotocol Peer
  (hydrate [this request])
  (db [this uri branch])

  ; used for clone-link
  (hydrate-one! [this request]))
