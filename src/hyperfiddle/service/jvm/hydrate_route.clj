(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [return mlet]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests stage-val->staged-branches]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop hydrate-loop-adapter]]
            [hyperfiddle.io.local-basis :refer [fetch-domain!]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


(deftype HydrateRoute [hyperfiddle-hostname hostname foo ide-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]
    ; Don't have a local basis, we do have a global-basis though
    (mlet [domain (fetch-domain! rt hostname hyperfiddle-hostname global-basis)]
      (return
        (let [ctx {:hyperfiddle-hostname hyperfiddle-hostname
                   :hostname hostname
                   :branch branch
                   :peer rt}]
          ; Local basis has to have enough info to call the API (even if we omit that call today)
          (foundation/local-basis foo global-basis encoded-route domain ctx
                                  (partial ide/local-basis foo))))))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname foo ide-repo (reactive/atom %))
                                             #(foundation/api foo encoded-route % (partial ide/api foo)))
                    local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis encoded-route stage]  ; encoded-route in state ?
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch nil
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname foo ide-repo (reactive/atom %))
                                             #(foundation/api "page" encoded-route % (partial ide/api "page")))
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
    (peer/db-pointer state-atom uri branch))

  hc/HydrateApi
  (hydrate-api [this request]
    (unwrap @(hc/hydrate this request)))

  ide/SplitRuntime
  (sub-rt [rt foo ide-repo]
    (HydrateRoute. hyperfiddle-hostname hostname foo ide-repo state-atom)))
