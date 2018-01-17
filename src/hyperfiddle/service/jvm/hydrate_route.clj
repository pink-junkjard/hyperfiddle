(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]] ; todo
            [hyperfiddle.appfn.sync :refer [sync]]          ; todo
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]] ; todo
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis]]
            [promesa.core :as p]

            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.ide]

    ; imports for user-land
            [hypercrud.ui.auto-control]
            [hypercrud.ui.form]
            [hypercrud.ui.table]

            [cats.monad.either :as either]
            [hypercrud.util.non-fatal :refer [try-either]]



            [hyperfiddle.appval.domain.core]
            [hyperfiddle.core]                              ; compat
            [taoensso.timbre :as timbre]
            [hypercrud.util.performance :as perf]))

(deftype HydrateRoute [hyperfiddle-hostname hostname foo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]
    ; Foundation local-basis is same for all foo.
    (foundation/local-basis (partial hyperfiddle.ide/local-basis foo) global-basis encoded-route))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])]
      (hydrate-route
        rt
        ; foundation/api doesn't care about page (foundation/view does though)
        (partial foundation/api (partial hyperfiddle.ide/api foo) hyperfiddle-hostname hostname encoded-route branch)
        local-basis branch stage data-cache)))

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
