(ns hyperfiddle.service.jvm.local-basis
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [mlet return]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.appfn.sync :refer [sync]]
            [hyperfiddle.appfn.runtime-local :refer [stage-val->staged-branches]]
            [hyperfiddle.appval.runtime-local :refer [global-basis fetch-domain!]]
            [promesa.core :as p]
            [hyperfiddle.appval.domain.foundation :as foundation]))


; This is allowed to hydrate route, this runtime is probably the same as hydrate-route runtime
(deftype LocalBasis [hyperfiddle-hostname hostname foo ide-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]
    (mlet [domain (fetch-domain! rt hostname hyperfiddle-hostname global-basis)]
      (return
        (let [ctx {:hostname hostname
                   :hyperfiddle-hostname hyperfiddle-hostname
                   :branch branch
                   :peer rt}]
          (foundation/local-basis foo global-basis encoded-route domain ctx
                                  (partial hyperfiddle.ide/local-basis foo))))))

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
    (peer/db-pointer state-atom uri branch))

  hyperfiddle.ide/SplitRuntime
  (sub-rt [rt foo ide-repo]
    (LocalBasis. hyperfiddle-hostname hostname foo ide-repo state-atom)))
