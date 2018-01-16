(ns hyperfiddle.service.node.hydrate-route
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route local-basis]]
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


(deftype HydrateRouteRuntime [hyperfiddle-hostname hostname service-uri state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis! service-uri))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt (partial hyperfiddle.ide/local-basis foo) hyperfiddle-hostname hostname global-basis encoded-route))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis encoded-route foo branch stage]
    (let [data-cache (select-keys @state-atom [:id->tempid :ptm])]
      (hydrate-route rt (partial foundation/api (partial hyperfiddle.ide/api foo))
                     hyperfiddle-hostname hostname local-basis encoded-route branch stage data-cache)))

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

  ; IEquiv?

  IHash
  (-hash [this] (goog/getUid this)))


(defn http-hydrate-route [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (req->state-val env req path-params query-params)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (HydrateRouteRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (runtime/hydrate-route rt (:local-basis state-val) (:encoded-route state-val) foo branch (:stage state-val))
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
