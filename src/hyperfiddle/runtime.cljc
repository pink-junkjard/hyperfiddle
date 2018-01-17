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

(defprotocol AppFnSsr
  (ssr [rt route]))

(defprotocol AppFnTransact!
  (transact! [rt tx-groups]))

; App val uses link-graph to determine a local-basis and optimize hydrates for cache locality

; Which local-basis? A page local-basis or an ide popover or user popover?
(defprotocol AppValLocalBasis                               ; only the data for this route, but decoupled from browser, foo not allowed.
  (local-basis [rt global-basis encoded-route branch]))

(defprotocol AppValHydrate
  ; user-data-fn not on this interface; hardcoded in runtime impls or read from db
  (hydrate-route [rt local-basis encoded-route branch stage]) ; returns ptm without stage-val hashes
  (hydrate-route-page [rt local-basis encoded-route branch stage]))
