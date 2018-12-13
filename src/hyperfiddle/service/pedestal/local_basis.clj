(ns hyperfiddle.service.pedestal.local-basis
  (:refer-clojure :exclude [sync])
  (:require
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [contrib.reactive :as r]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.hydrate-requests :refer [stage-val->staged-branches]]
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.pedestal.interceptors :refer [platform->pedestal-req-handler]]
    [hyperfiddle.state :as state]
    [promesa.core :as p]))


; This is allowed to hydrate route, this runtime is probably the same as hydrate-route runtime
(deftype RT [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/IO
  (global-basis [rt]
    (global-basis rt (:domain-eid host-env)))

  (local-basis [rt branch]
    (let [global-basis @(runtime/state rt [::runtime/global-basis])
          {:keys [route ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          ctx {:branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  (hydrate-requests [rt local-basis stage requests]
    ;(http/hydrate-requests! service-uri local-basis stage requests)
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches ?subject))))

  (sync [rt dbs]
    (p/do* (sync dbs)))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch)))

(defmethod handle-route :local-basis [handler env req]
  (platform->pedestal-req-handler (partial http-service/local-basis-handler ->RT) req))
