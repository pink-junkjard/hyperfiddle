(ns hypercrud.client.core)


(def ^:dynamic *root-conn-id* nil)

(defprotocol Response
  (hydrate [this request])
  (db [this conn-id branch])
  (tx [this db]))


(defprotocol Peer
  (hydrate! [this request])                                 ; hydrate a full page
  (transact! [this conn-id])                                   ; push - stage first as a separate step.

  ; just need branch identity, don't need a hydrated db value
  (with! [this conn-id branch tx])                             ; stage datoms in a branch
  (merge! [this conn-id branch])                               ; merge a branch to parent
  (discard! [this conn-id branch])

  ; for UIs
  (hydrated? [this request])

  ; internal & used for clone-link
  (hydrate!* [this request])
  (hydrate-one! [this request]))
