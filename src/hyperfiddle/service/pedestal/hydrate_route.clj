(ns hyperfiddle.service.pedestal.hydrate-route
  (:refer-clojure :exclude [sync])
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
    [hyperfiddle.io.sync :refer [sync]]
    [hyperfiddle.io.util :refer [process-result]]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.service.http :as http-service :refer [handle-route]]
    [hyperfiddle.service.pedestal.interceptors :refer [platform->pedestal-req-handler]]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(deftype RequestFn [db-with-lookup get-secure-db-with+ host-env state-atom root-reducer jwt ?subject]
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
            (let [response (hydrate-requests/hydrate-request get-secure-db-with+ request ?subject)
                  ptm (assoc ptm request response)
                  tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup branch)]
              (runtime/dispatch! rt [:hydrate!-success branch ptm tempid-lookups])
              (get ptm request)))
          (process-result request)
          (r/atom))))

  (db [this uri branch]
    (peer/db-pointer uri branch)))

(deftype RT [host-env state-atom root-reducer jwt ?subject]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/IO
  (hydrate-route [rt branch]
    (let [{:keys [route local-basis ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          stage (map-values :stage @(runtime/state rt [::runtime/partitions]))
          local-basis (->> (mapcat second stage)            ; todo lift this up into hydrate-route-handler, need to solve domain/user tension first
                           (remove (comp empty? second))
                           (map first)
                           (distinct)
                           ; todo verify validity of uris?
                           (sync)
                           (reduce-kv assoc (into {} local-basis)))
          _ (runtime/dispatch! rt [:partition-basis branch local-basis]) ; no one uses this but state should be consistent
          db-with-lookup (atom {})
          get-secure-db-with+ (hydrate-requests/build-get-secure-db-with+ (stage-val->staged-branches stage) db-with-lookup local-basis)
          ; todo should we be sharing state-atom?
          rt (->RequestFn db-with-lookup get-secure-db-with+ host-env state-atom root-reducer jwt ?subject)
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
                   (get-secure-db-with+ uri branch-ident)))
      (perf/time (fn [get-total-time] (timbre/debug "Hydrate-route::request-fn" "total time: " (get-total-time)))
                 (foundation/api page-or-leaf ctx (partial ide/api route)))
      (p/resolved (select-keys @(runtime/state rt [::runtime/partitions branch]) [:local-basis :ptm :tempid-lookups]))))

  (hydrate-requests [rt local-basis stage requests]
    {:pre [requests (not-any? nil? requests)]}
    (let [staged-branches (stage-val->staged-branches stage)]
      (p/resolved (hydrate-requests/hydrate-requests local-basis requests staged-branches ?subject))))

  runtime/Schema
  (hydrate-schemas [rt branch]
    (ide/hydrate-schemas rt branch))

  hc/Peer
  (db [this uri branch]
    (peer/db-pointer uri branch)))

(defmethod handle-route :hydrate-route [handler env req]
  (platform->pedestal-req-handler (partial http-service/hydrate-route-handler ->RT) req))
