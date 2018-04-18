(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [contrib.data :refer [unwrap]]
            [contrib.reactive :as reactive]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests stage-val->staged-branches]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop request-fn-adapter]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.state :as state]
            [promesa.core :as p]))


(deftype HydrateRoute [hyperfiddle-hostname hostname service-uri state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (reactive/cursor state-atom path))

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      ; Local basis has to have enough info to call the API (even if we omit that call today)
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis route branch branch-aux stage]
    {:pre [route (not (string? route))]}
    (let [data-cache (-> @(runtime/state rt [::runtime/partitions branch]) (select-keys [:tempid-lookups :ptm]))
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (hydrate-loop rt (request-fn-adapter local-basis route stage ctx
                                           #(HydrateRoute. hyperfiddle-hostname hostname service-uri (reactive/atom %) root-reducer)
                                           #(foundation/api page-or-leaf route % ide/api))
                    local-basis branch stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    {:pre [requests (not-any? nil? requests)]}
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches))))

  runtime/AppFnSync
  (sync [rt dbs]
    (p/do* (sync dbs)))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this branch request]
    (unwrap @(hc/hydrate this branch request))))
