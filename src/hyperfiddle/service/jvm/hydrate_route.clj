(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.api :as api]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]] ; todo
            [hyperfiddle.appfn.sync :refer [sync]] ; todo
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]] ; todo
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync! transact!!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [hydrate-route! global-basis! local-basis!]]
            [promesa.core :as p]

    ; imports for user-land
            [hypercrud.ui.auto-control]
            [hypercrud.ui.form]
            [hypercrud.ui.table]
            [hyperfiddle.appval.domain.core]
            [hyperfiddle.ide.cljc]
            [hyperfiddle.core]                              ; compat
            ))


(deftype HydrateRoute [hyperfiddle-hostname hostname state-atom]
  api/AppValGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  api/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt hyperfiddle-hostname hostname global-basis encoded-route foo))

  api/AppValHydrate
  (hydrate-route [rt local-basis encoded-route foo branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])]
      (hydrate-route rt hyperfiddle-hostname hostname local-basis encoded-route foo branch stage data-cache)))

  api/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests local-basis requests staged-branches))))

  api/AppFnSync
  (sync [rt dbs]
    (p/resolved (sync dbs)))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer state-atom uri branch)))
