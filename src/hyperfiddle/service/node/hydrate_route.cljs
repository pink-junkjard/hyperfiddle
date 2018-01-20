(ns hyperfiddle.service.node.hydrate-route
  (:require [cats.core :refer [mlet return]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.core :refer [unwrap]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync!]]
            [hyperfiddle.appfn.runtime-local :refer [hydrate-loop]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-loop-adapter fetch-domain!]]
            [hyperfiddle.appval.runtime-rpc :refer [global-basis!]]

            [hyperfiddle.service.node.lib :refer [req->service-uri req->state-val]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.reactive :as reactive]
            [promesa.core :as p]

            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.ide]
            ))


(deftype HydrateRouteRuntime [hyperfiddle-hostname hostname service-uri foo ide-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis! service-uri))

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

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route branch stage] ; :: ... -> DataCache on the wire
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo ide-repo (reactive/atom %))
                                             #(foundation/api foo encoded-route % (partial hyperfiddle.ide/api foo)))
                    local-basis stage data-cache)))

  (hydrate-route-page [rt local-basis encoded-route stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch nil
               :peer rt}]
      (hydrate-loop rt (hydrate-loop-adapter local-basis stage ctx
                                             #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo ide-repo (reactive/atom %))
                                             #(foundation/api "page" encoded-route % (partial hyperfiddle.ide/api "page")))
                    local-basis stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests! service-uri local-basis stage requests))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync! service-uri dbs))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer state-atom uri branch))

  hc/HydrateApi
  (hydrate-api [this request]
    (unwrap (hc/hydrate this request)))

  hyperfiddle.ide/SplitRuntime
  (sub-rt [rt foo ide-repo]
    (HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri foo ide-repo state-atom))

  IHash
  (-hash [this] (goog/getUid this)))


(defn http-hydrate-route [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (req->state-val env req path-params query-params)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (HydrateRouteRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) foo nil (reactive/atom state-val))]
    (-> (runtime/hydrate-route rt (:local-basis state-val) (:encoded-route state-val) branch (:stage state-val))
        (p/then (fn [data]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=31536000") ; todo max-age=0 if POST
                    (.format #js {"application/transit+json" #(.send res (transit/encode data))
                                  #_"text/html" #_(fn []
                                                    (mlet [html-fragment nil #_(api-impl/local-html ui-fn ctx)]
                                                          (.send res html-fragment)))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     ; todo caching on errors, there are a subset of requests that are actually permanently cacheable
                     (.format #js {"application/transit+json" #(.send res (transit/encode {:error (pr-str error)}))
                                   #_"text/html" #_(fn []
                                                     (document/error error))})))))))
