(ns hyperfiddle.service.jvm.hydrate-route
  (:refer-clojure :exclude [sync])
  (:require
    [contrib.performance :as perf]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.io.global-basis :refer [global-basis]]
    [hyperfiddle.io.hydrate-requests :refer [stage-val->staged-branches]]
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.io.util :refer [process-result]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(deftype HydrateRoute [db-with-lookup ^:unsynchronized-mutable get-secure-db-with ; oof this is spaghetti
                       host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt (:domain-eid host-env)))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:branch branch
               ::runtime/branch-aux branch-aux
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
    (set! get-secure-db-with (hydrate-requests/build-get-secure-db-with (stage-val->staged-branches stage) db-with-lookup (into {} local-basis)))
    ; must d/with at the beginning otherwise tempid reversal breaks
    (doseq [[branch-ident branch-content] stage
            [uri _] branch-content]
      ; todo only execute once per uri (for leaf branch, parents are implied)
      (get-secure-db-with uri branch-ident))
    (let [ctx {:branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (perf/time (fn [get-total-time] (timbre/debug "Hydrate-route" "total time: " (get-total-time)))
                 (foundation/api page-or-leaf ctx (partial ide/api route)))
      (p/resolved (select-keys @(runtime/state rt [::runtime/partitions branch]) [:tempid-lookups :ptm]))))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    {:pre [requests (not-any? nil? requests)]}
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests/hydrate-requests local-basis requests staged-branches ?subject))))

  runtime/AppFnSync
  (sync [rt dbs]
    (p/do* (sync dbs)))

  hc/Peer
  (hydrate [rt branch request]
    ; this MUST only be called from the request fn, otherwise non-route state will bleed in
    (let [ptm @(runtime/state rt [::runtime/partitions branch :ptm])]
      (-> (if (contains? ptm request)
            (get ptm request)
            (let [response (hydrate-requests/hydrate-request get-secure-db-with request ?subject)
                  ptm (assoc ptm request response)
                  tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup branch)]
              (runtime/dispatch! rt [:hydrate!-success branch ptm tempid-lookups])
              (get ptm request)))
          (process-result request)
          (r/atom))))

  (db [this uri branch]
    (peer/db-pointer uri branch)))
