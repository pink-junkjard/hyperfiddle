(ns hyperfiddle.service.ssr.runtime
  (:require
    [contrib.data :refer [map-values]]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
    [hyperfiddle.io.hydrate-route :refer [hydrate-route-rpc!]]
    [hyperfiddle.io.sync :refer [sync-rpc!]]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]))


(deftype ReferenceImpl [host-env state-atom root-reducer jwt]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! (:service-uri host-env) (:build host-env) jwt))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/AppValLocalBasis
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

  runtime/AppValHydrate
  (hydrate-route [rt branch]
    (let [{:keys [route local-basis ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          stage (map-values :stage @(runtime/state rt [::runtime/partitions]))]
      (hydrate-route-rpc! (:service-uri host-env) (:build host-env) local-basis route branch branch-aux stage jwt)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! (:service-uri host-env) (:build host-env) local-basis stage requests jwt))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! (:service-uri host-env) (:build host-env) dbs jwt))

  runtime/Schema
  (hydrate-schemas [rt branch]
    (ide/hydrate-schemas rt branch))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  IHash
  (-hash [this] (goog/getUid this)))

(defn build [host-env initial-state jwt]
  (->ReferenceImpl host-env (r/atom (reducers/root-reducer initial-state nil)) reducers/root-reducer jwt))
