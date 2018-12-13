(ns hyperfiddle.service.pedestal.global-basis
  (:refer-clojure :exclude [sync])
  (:require
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.hydrate-requests :refer [stage-val->staged-branches]]
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.pedestal.interceptors :refer [platform->pedestal-req-handler]]
    [hyperfiddle.state :as state]
    [promesa.core :as p]))


(deftype RT [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/IO
  (global-basis [rt]
    (global-basis rt (:domain-eid host-env)))

  (hydrate-requests [rt local-basis stage requests]
    ;(hydrate-requests! service-uri local-basis stage requests)
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches ?subject))))

  (sync [rt dbs]
    (p/do* (sync dbs)))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch)))

(defmethod handle-route :global-basis [handler env req]
  (platform->pedestal-req-handler (partial http-service/global-basis-handler ->RT) req))
