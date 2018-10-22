(ns hyperfiddle.service.node.global-basis
  (:require [contrib.reactive :as r]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.state :as state]))


(deftype GlobalBasisRuntime [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt (:domain-eid host-env)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! (:service-uri host-env) (:build host-env) local-basis stage requests jwt))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! (:service-uri host-env) (:build host-env) dbs jwt))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  ; IEquiv?

  IHash
  (-hash [this] (goog/getUid this)))
