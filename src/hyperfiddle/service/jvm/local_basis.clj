(ns hyperfiddle.service.jvm.local-basis
  (:refer-clojure :exclude [sync])
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]] ; todo
            [hyperfiddle.appfn.sync :refer [sync]]          ; todo
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]] ; todo
            [hyperfiddle.appval.runtime-local :refer [global-basis]]
            [promesa.core :as p]
            [hyperfiddle.appval.domain.foundation :as foundation]))


(deftype LocalBasis [hyperfiddle-hostname hostname foo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]
    ; This is allowed to hydrate route, this runtime is probably the same as hydrate-route runtime
    (let [ctx {:dispatch! #()
               :hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt
               :peer-ide (LocalBasis. hyperfiddle-hostname hostname "ide" state-atom)
               :peer-user (LocalBasis. hyperfiddle-hostname hostname "user" state-atom)}]
      (foundation/local-basis foo global-basis encoded-route ctx
                              (partial hyperfiddle.ide/local-basis foo))))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    ;(http/hydrate-requests! service-uri local-basis stage requests)
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
