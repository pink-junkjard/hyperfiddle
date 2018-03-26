(ns hyperfiddle.service.node.hydrate-route
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.core :refer [unwrap]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop request-fn-adapter]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [hyperfiddle.state :as state]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(deftype HydrateRouteRuntime [hyperfiddle-hostname hostname service-uri state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (reactive/cursor state-atom path))

  ;runtime/AppFnGlobalBasis
  ;(global-basis [rt]
  ;  (global-basis-rpc! service-uri))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis route branch branch-aux stage] ; :: ... -> DataCache on the wire
    (let [data-cache (-> @(runtime/state rt [::runtime/partitions branch]) (select-keys [:tempid-lookups :ptm]))
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (hydrate-loop rt (request-fn-adapter local-basis route stage ctx
                                           #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri (reactive/atom %) root-reducer)
                                           #(foundation/api page-or-leaf route % ide/api))
                    local-basis branch stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! service-uri local-basis stage requests))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! service-uri dbs))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this branch request]
    (unwrap @(hc/hydrate this branch request)))

  IHash
  (-hash [this] (goog/getUid this)))


(defn http-hydrate-route [env req res path-params query-params]
  (try
    (let [hostname (.-hostname req)
          branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
          branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode reader/read-edn-string)
          local-basis (-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
          route (-> (:encoded-route path-params) base-64-url-safe/decode reader/read-edn-string)
          initial-state {::stage (some-> req .-body lib/hack-buggy-express-body-text-parser transit/decode)
                         :user-profile (lib/req->user-profile env req)
                         ::runtime/partitions {branch {:local-basis local-basis
                                                       :route route
                                                       :hyperfiddle.runtime/branch-aux branch-aux}}}
          rt (->HydrateRouteRuntime (:HF_HOSTNAME env) hostname (lib/req->service-uri env req)
                                    (reactive/atom (reducers/root-reducer initial-state nil))
                                    reducers/root-reducer)]
      (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then #(runtime/hydrate-route rt local-basis route branch branch-aux (:stage initial-state)))
          (p/then (fn [data]
                    (doto res
                      (.status 200)
                      (.append "Cache-Control" "max-age=31536000") ; todo max-age=0 if POST
                      (.format #js {"application/transit+json" #(.send res (transit/encode data))
                                    #_"text/html" #_(fn []
                                                      (mlet [html-fragment nil #_(api-impl/local-html ui-fn ctx)]
                                                            (.send res html-fragment)))}))))
          (p/catch (fn [e]
                     (timbre/error e)
                     (lib/e->response res e)))))
    (catch :default e
      (timbre/error e)
      (lib/e->response res e))))
