(ns hypercrud.client.core)


(defprotocol Peer
  (hydrate [this branch request])
  (db [this uri branch]))

(defprotocol HydrateApi
  (hydrate-api [this branch request]))
