(ns hyperfiddle.service.jvm.global-basis
  (:refer-clojure :exclude [sync])
  (:require [contrib.reactive :as r]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [stage-val->staged-branches]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.state :as state]
            [promesa.core :as p]))


(deftype GlobalBasisRuntime [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt (:domain-eid host-env)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    ;(hydrate-requests! service-uri local-basis stage requests)
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches ?subject))))

  runtime/AppFnSync
  (sync [rt dbs]
    (p/do* (sync dbs)))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch)))
