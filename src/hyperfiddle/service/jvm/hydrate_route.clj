(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [return mlet]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.performance :as perf]
            [hypercrud.util.core :refer [unwrap]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.appfn.sync :refer [sync]]
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches hydrate-loop]]
            [hyperfiddle.appval.runtime-local :refer [global-basis hydrate-loop-adapter fetch-domain!]]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide]
            [hyperfiddle.core]                              ; compat

            [promesa.core :as p]
            [taoensso.timbre :as timbre]

    ; imports for user-land
            [hypercrud.ui.auto-control]
            [hypercrud.ui.form]
            [hypercrud.ui.table]
            [hypercrud.util.reactive :as reactive]))


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
                                  (partial hyperfiddle.ide/local-basis foo))))))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname foo ide-repo (reactive/atom %))
                                             #(foundation/api foo encoded-route % (partial hyperfiddle.ide/api foo)))
                    local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis encoded-route stage]  ; encoded-route in state ?
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch nil
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRoute. hyperfiddle-hostname hostname foo ide-repo (reactive/atom %))
                                             #(foundation/api "page" encoded-route % (partial hyperfiddle.ide/api "page")))
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

  hyperfiddle.ide/SplitRuntime
  (sub-rt [rt foo ide-repo]
    (HydrateRoute. hyperfiddle-hostname hostname foo ide-repo state-atom)))
