(ns hypercrud.client.core)


(defprotocol Peer
  (hydrate [this request])
  (db [this uri branch])

  ; used for clone-link
  (hydrate-one! [this request]))
