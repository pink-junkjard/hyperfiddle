(ns hyperfiddle.runtime
  (:refer-clojure :exclude [sync]))

; App fn - just Datomic primitives
; "api" vs "fiddle"

(defprotocol AppFnGlobalBasis                               ; anything reachable through navigation; so likely all dbs in :domain/env
  (global-basis [rt]))

(defprotocol AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]))

(defprotocol AppFnSync
  (sync [rt dbs]))

(defprotocol AppFnRenderPageRoot                            ; Same protocol is Client Render
  (ssr [rt route]))

(defprotocol AppFnTransact!
  (transact! [rt tx-groups]))

; App val uses link-graph to determine a local-basis and optimize hydrates for cache locality

(defprotocol AppValLocalBasis                               ; only the data for this route, but decoupled from browser, foo not allowed.
  (local-basis [rt global-basis route branch])
  (local-basis-page [rt global-basis route]))

(defprotocol AppValHydrate
  ; user-data-fn not on this interface; hardcoded in runtime impls or read from db
  (hydrate-route [rt local-basis #_"actions need to set this without a new instance" route branch stage]) ; returns ptm without stage-val hashes
  (hydrate-route-page [rt local-basis route stage]))

#_(defprotocol State
  (dispatch! [rt])
  (get-state [rt]))

(defprotocol Route
  ; let the call site sort out how to get domain-basis.
  ; maybe from global-basis, maybe from local-basis, depends what we were sent up.
  (encode-route [rt v])
  (decode-route [rt s]))

(defprotocol DomainRegistry
  (domain [rt]))
