(ns hypercrud.client.core)


(defprotocol Peer
  (hydrate [this request])
  (db [this uri branch]))

(defprotocol HydrateApi
  (hydrate-api [this request]))
