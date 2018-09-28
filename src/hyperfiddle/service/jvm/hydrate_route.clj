(ns hyperfiddle.service.jvm.hydrate-route
  (:require
    [contrib.data :refer [map-values]]
    [contrib.performance :as perf]
    [contrib.reactive :as r]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.io.hydrate-requests :refer [stage-val->staged-branches]]
    [hyperfiddle.io.util :refer [process-result]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(deftype RequestFn [db-with-lookup get-secure-db-with host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  hc/Peer
  (hydrate [rt branch request]
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

(deftype HydrateRoute [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/AppValHydrate
  (hydrate-route [rt branch]
    (let [{:keys [route local-basis ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          stage (map-values :stage @(runtime/state rt [::runtime/partitions]))
          db-with-lookup (atom {})
          get-secure-db-with (hydrate-requests/build-get-secure-db-with (stage-val->staged-branches stage) db-with-lookup (into {} local-basis))
          ; todo should we be sharing state-atom?
          rt (->RequestFn db-with-lookup get-secure-db-with host-env state-atom root-reducer jwt ?subject)
          ctx {:branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (perf/time (fn [get-total-time] (timbre/debug "Hydrate-route::d/with" "total time: " (get-total-time)))
                 ; must d/with at the beginning otherwise tempid reversal breaks
                 (doseq [[branch-ident branch-content] stage
                         [uri _] branch-content]
                   ; todo only execute once per uri (for leaf branch, parents are implied)
                   (get-secure-db-with uri branch-ident)))
      (perf/time (fn [get-total-time] (timbre/debug "Hydrate-route::request-fn" "total time: " (get-total-time)))
                 (foundation/api page-or-leaf ctx (partial ide/api route)))
      (p/resolved (select-keys @(runtime/state rt [::runtime/partitions branch]) [:tempid-lookups :ptm]))))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    {:pre [requests (not-any? nil? requests)]}
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests/hydrate-requests local-basis requests staged-branches ?subject))))

  hc/Peer
  (db [this uri branch]
    (peer/db-pointer uri branch)))
