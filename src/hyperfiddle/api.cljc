(ns hyperfiddle.api
  (:refer-clojure :exclude [sync]))


(defprotocol AppValGlobalBasis
  (global-basis [rt]))

(defprotocol AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]))

(defprotocol AppValHydrate
  ; returns ptm without stage-val hashes
  (hydrate-route [rt local-basis encoded-route foo branch stage]))

(defprotocol AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]))

(defprotocol AppFnSync
  (sync [rt dbs]))

(defprotocol AppFnTransact!
  (transact! [rt tx-groups]))
