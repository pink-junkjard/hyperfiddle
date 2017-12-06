(ns hypercrud.client.core)


(defprotocol Peer
  (hydrate [this request])
  (db [this uri branch]))
