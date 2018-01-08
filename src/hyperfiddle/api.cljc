(ns hyperfiddle.api
  (:refer-clojure :exclude [sync]))


(defprotocol AppFnApi
  (hydrate-requests [rt local-basis stage requests])
  (sync [rt dbs])
  (transact! [rt tx-groups]))

(defprotocol AppValApi
  (global-basis [rt])
  (local-basis [rt global-basis encoded-route foo branch])
  (hydrate-route [rt local-basis encoded-route foo branch stage])) ; returns ptm without stage-val hashes

;(defprotocol AppValGlobalBasis
;  (global-basis [rt]))
;(defprotocol AppValLocalBasis
;  (local-basis [rt global-basis encoded-route foo branch]))
;(defprotocol AppValHydrate
;  (hydrate-route [rt local-basis encoded-route foo branch stage]))
;(defprotocol AppFnHydrate
;  (hydrate-requests [rt local-basis stage requests]))
;(defprotocol AppFnSync
;  (sync [rt dbs]))
;(defprotocol AppFnTransact!
;  (transact! [rt tx-groups]))