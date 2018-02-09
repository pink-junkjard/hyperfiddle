(ns hyperfiddle.service.node.hydrate-route
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
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
            [hyperfiddle.ide-rt :as ide-rt]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop hydrate-loop-adapter]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [hyperfiddle.state :as state]
            [promesa.core :as p]))


(deftype HydrateRouteRuntime [hyperfiddle-hostname hostname service-uri foo target-repo state-atom]
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
  (local-basis [rt global-basis route branch]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               :peer rt}]
      (foundation/local-basis foo global-basis route ctx (partial ide/local-basis foo))))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route branch stage] ; :: ... -> DataCache on the wire
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo target-repo (reactive/atom %))
                                             #(foundation/api foo encoded-route % (partial ide/api foo)))
                    local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis encoded-route stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch nil
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo target-repo (reactive/atom %))
                                             #(foundation/api "page" encoded-route % (partial ide/api "page")))
                    local-basis stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! service-uri local-basis stage requests))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! service-uri dbs))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this request]
    (unwrap @(hc/hydrate this request)))

  ide-rt/SplitRuntime
  (sub-rt [rt foo target-repo]
    (HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo target-repo state-atom))
  (target-repo [rt] target-repo)

  IHash
  (-hash [this] (goog/getUid this)))


(defn http-hydrate-route [env req res {:keys [encoded-route] :as path-params} query-params]
  (let [hostname (.-hostname req)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        target-repo (some-> (:target-repo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        local-basis (-> (:local-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        route (routing/decode encoded-route)
        initial-state {:local-basis local-basis             ; no need for :route here, it is ignored, this whole thing gets clobbered
                       :stage (some-> req .-body lib/hack-buggy-express-body-text-parser transit/decode)
                       :user-profile (lib/req->user-profile env req)}]
    ; this inner let should reduce to foundation/bootstrap-data
    (let [state-atom (reactive/atom (reducers/root-reducer initial-state nil))
          rt (->HydrateRouteRuntime (:HF_HOSTNAME env) hostname (lib/req->service-uri env req) foo target-repo state-atom)
          dispatch! (state/build-dispatch state-atom reducers/root-reducer)
          get-state (fn [] @state-atom)]
      (-> (foundation-actions/refresh-domain rt dispatch! get-state)
          (p/then #(runtime/hydrate-route rt local-basis route branch (:stage initial-state)))
          (p/then (fn [data]
                    (doto res
                      (.status 200)
                      (.append "Cache-Control" "max-age=31536000") ; todo max-age=0 if POST
                      (.format #js {"application/transit+json" #(.send res (transit/encode data))
                                    #_"text/html" #_(fn []
                                                      (mlet [html-fragment nil #_(api-impl/local-html ui-fn ctx)]
                                                            (.send res html-fragment)))}))))
          (p/catch
            (fn [error]
              (doto res
                (.status 500)
                ; todo caching on errors, there are a subset of requests that are actually permanently cacheable
                (.format #js {"application/transit+json" #(.send res (transit/encode {:error (pr-str error)}))
                              #_"text/html" #_(fn []
                                                (document/error error))}))))))))
