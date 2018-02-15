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
  (local-basis [rt global-basis route branch branch-aux])
  ; interface could just be
  #_(local-basis [rt branch-ident]))

(defprotocol AppValHydrate
  ; user-data-fn not on this interface; hardcoded in runtime impls or read from db
  ; returns ptm without stage-val hashes
  (hydrate-route [rt local-basis route branch branch-aux stage])
  ; interface could just be
  #_(hydrate-route [rt branch-ident]))

(defprotocol State
  (dispatch! [rt action-or-func])
  (state [rt] [rt path]))

(defprotocol Route
  ; let the call site sort out how to get domain-basis.
  ; maybe from global-basis, maybe from local-basis, depends what we were sent up.
  (encode-route [rt v])
  (decode-route [rt s]))

(defprotocol DomainRegistry
  (domain [rt]))
