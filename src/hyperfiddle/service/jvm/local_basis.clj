(ns hyperfiddle.service.jvm.local-basis
  (:refer-clojure :exclude [sync])
  (:require [cats.core :refer [mlet return]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.ide-rt :as ide-rt]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests stage-val->staged-branches]]
            [hyperfiddle.io.sync :refer [sync]]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]))


; This is allowed to hydrate route, this runtime is probably the same as hydrate-route runtime
(deftype LocalBasis [hyperfiddle-hostname hostname domain foo target-repo state-atom]
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
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               :peer rt}]
      (foundation/local-basis foo global-basis route domain ctx
                              (partial ide/local-basis foo))))

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
    (peer/db-pointer uri branch))

  ide-rt/SplitRuntime
  (sub-rt [rt foo target-repo]
    (LocalBasis. hyperfiddle-hostname hostname domain foo target-repo state-atom))
  (target-repo [rt] target-repo))
