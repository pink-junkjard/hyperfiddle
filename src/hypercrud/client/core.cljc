(ns hypercrud.client.core)


(defprotocol Peer
  (hydrate [this branch request]))
