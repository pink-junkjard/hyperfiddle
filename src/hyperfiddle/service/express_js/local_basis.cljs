(ns hyperfiddle.service.express-js.local-basis
  (:require
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
    [hyperfiddle.io.sync :refer [sync-rpc!]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.service.express-js.middleware :refer [platform->express-req-handler]]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.state :as state]))


; Todo this is same runtime as HydrateRoute
(deftype Runtime [host-env state-atom root-reducer jwt ?subject]
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
    (global-basis-rpc! (:service-uri host-env) (:build host-env) jwt))

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
    (hydrate-requests-rpc! (:service-uri host-env) (:build host-env) local-basis stage requests jwt))

  (sync [rt dbs]
    (sync-rpc! (:service-uri host-env) (:build host-env) dbs jwt))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  IHash
  (-hash [this] (goog/getUid this)))

(defmethod handle-route :local-basis [handler env req res]
  (platform->express-req-handler (partial http-service/local-basis-handler ->Runtime) req res))
