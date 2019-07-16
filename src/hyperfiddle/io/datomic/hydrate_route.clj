(ns hyperfiddle.io.datomic.hydrate-route
  (:require
    [cats.core :refer [alet]]
    [cats.monad.either :as either]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.io.legacy :refer [stage->staged-branches]]
    [hyperfiddle.project :as project]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(deftype RT [domain db-with-lookup get-secure-db-with+ state-atom ?subject]
  runtime/State
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HF-Runtime
  (domain [rt] domain)
  (hydrate [rt branch request]
    (let [ptm @(runtime/state rt [::runtime/partitions branch :ptm])]
      (-> (if (contains? ptm request)
            (get ptm request)
            (let [response (hydrate-requests/hydrate-request domain get-secure-db-with+ request ?subject)
                  ptm (assoc ptm request response)
                  tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup branch)]
              (runtime/dispatch! rt [:hydrate!-success branch ptm tempid-lookups])
              response))
          (r/atom)))))

(defn hydrate-route [domain local-basis route branch stage ?subject]
  (let [staged-branches (stage->staged-branches stage)
        aux-io (reify io/IO
                 (hydrate-requests [io local-basis staged-branches requests]
                   (p/do* (hydrate-requests/hydrate-requests domain local-basis requests staged-branches ?subject))))]
    (alet [schemas (schema/hydrate-schemas aux-io domain local-basis branch staged-branches)
           ; schemas can NEVER short the whole request
           ; if the fiddle-db is broken (duplicate datoms), then attr-renderers and project WILL short it
           attr-renderers (project/hydrate-attr-renderers aux-io domain local-basis branch staged-branches)
           project (project/hydrate-project-record aux-io domain local-basis branch staged-branches)]
      (let [db-with-lookup (atom {})
            get-secure-db-with+ (hydrate-requests/build-get-secure-db-with+ domain staged-branches db-with-lookup local-basis)]
        (perf/time (fn [total-time] (timbre/debugf "d/with total time: %sms" total-time))
                   ; must d/with at the beginning otherwise tempid reversal breaks
                   (doseq [[branch-ident branch-content] stage
                           [dbname _] branch-content]
                     (get-secure-db-with+ dbname branch-ident)))
        (let [initial-state (reduce (fn [state [branch v]]
                                      (assoc-in state [::runtime/partitions branch :stage] v))
                                    {::runtime/user-id ?subject
                                     ; should this be constructed with reducers?
                                     ; why dont we need to preheat the tempid lookups here for parent branches?
                                     ::runtime/partitions {branch {:attr-renderers attr-renderers
                                                                   :local-basis local-basis
                                                                   :project project ; todo this is needed once total, not once per partition
                                                                   :route route
                                                                   :schemas schemas
                                                                   :tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup branch)}}}
                                    stage)
              state-atom (r/atom (reducers/root-reducer initial-state nil))
              rt (->RT domain db-with-lookup get-secure-db-with+ state-atom ?subject)]
          (perf/time (fn [total-time]
                       (timbre/debugf "request function %sms" total-time)
                       (when (> total-time 500)
                         (timbre/warnf "Slow request function %sms :: route: %s" total-time route)))
                     (-> (base/browse-route+ (map->Context {:ident nil :branch branch :peer rt}) route)
                         (either/branch
                           (fn [e] (timbre/warn e))
                           browser-request/requests)))
          (select-keys @(runtime/state rt [::runtime/partitions branch]) [:local-basis :attr-renderers :project :ptm :schemas :tempid-lookups]))))))
