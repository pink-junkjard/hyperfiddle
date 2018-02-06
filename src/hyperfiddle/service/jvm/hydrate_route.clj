(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [return mlet]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.ide-rt :as ide-rt]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests stage-val->staged-branches]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop hydrate-loop-adapter]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


(deftype HydrateRoute [hyperfiddle-hostname hostname domain foo target-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/Route
  (decode-route [rt foo s]
    (ide/route-decode foo domain s))

  (encode-route [rt foo v]
    (ide/route-encode foo domain v))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch]
    ; Don't have a local basis, we do have a global-basis though
    (let [ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      ; Local basis has to have enough info to call the API (even if we omit that call today)
      (foundation/local-basis foo global-basis route domain ctx
                              (partial ide/local-basis foo))))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis ?route branch stage]
    {:pre [?route (not (string? ?route))]}
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname domain foo target-repo (reactive/atom %))
                                             #(foundation/api foo ?route % (partial ide/api foo)))
                    local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis route stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch nil
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname domain foo target-repo (reactive/atom %))
                                             #(foundation/api "page" route % (partial ide/api "page")))
                    local-basis stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    {:pre [requests (not-any? nil? requests)]}
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches))))

  runtime/AppFnSync
  (sync [rt dbs]
    (p/resolved (sync dbs)))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this request]
    (unwrap @(hc/hydrate this request)))

  ide-rt/SplitRuntime
  (sub-rt [rt foo target-repo]
    (HydrateRoute. hyperfiddle-hostname hostname domain foo target-repo state-atom))
  (target-repo [rt] target-repo))
