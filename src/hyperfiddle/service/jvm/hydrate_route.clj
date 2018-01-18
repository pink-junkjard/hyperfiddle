(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.performance :as perf]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]] ; todo
            [hyperfiddle.appfn.sync :refer [sync]]          ; todo
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]] ; todo
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis]]
            [hyperfiddle.appval.domain.core]
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.ide]
            [hyperfiddle.core]                              ; compat

            [promesa.core :as p]
            [taoensso.timbre :as timbre]

    ; imports for user-land
            [hypercrud.ui.auto-control]
            [hypercrud.ui.form]
            [hypercrud.ui.table]))

(deftype HydrateRoute [hyperfiddle-hostname hostname ide-or-user target-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]       ; Problem: use encoded-route in state for symmetry
    (let [ctx {:dispatch! #()
               :hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt
               :peer-ide nil
               :peer-user nil}]
      ; Local basis has to have enough info to call the API (even if we omit that call today)
      (foundation/local-basis ide-or-user (partial hyperfiddle.ide/local-basis ide-or-user target-repo)
                              global-basis ctx)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:dispatch! #()
               :hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt ; Blast peer every time
               :peer-ide nil
               :peer-user nil}]
      (hydrate-route rt (partial foundation/api ide-or-user (partial hyperfiddle.ide/api ide-or-user) ctx)
                     local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis encoded-route branch stage] ; encoded-route in state ?
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:dispatch! #()
               :hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt                                     ; blast peer every time
               :peer-ide nil
               :peer-user nil}]
      (hydrate-route rt (partial foundation/api "page" (partial hyperfiddle.ide/api "page") ctx)
                     local-basis stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches))))

  runtime/AppFnSync
  (sync [rt dbs]
    (p/resolved (sync dbs)))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer state-atom uri branch)))
