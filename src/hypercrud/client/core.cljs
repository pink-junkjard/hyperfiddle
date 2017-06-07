(ns hypercrud.client.core)


(def ^:dynamic *root-conn-id* nil)

(defprotocol Response
  (hydrate [this request])
  (db [this conn-id branch])
  (tx [this db]))


(defprotocol Peer
  (hydrate! [this request])                                 ; hydrate a full page
  (transact! [this])                                        ; push - stage first as a separate step.

  ; for UIs
  (hydrated? [this request])

  ; used for clone-link
  (hydrate-one! [this request]))
