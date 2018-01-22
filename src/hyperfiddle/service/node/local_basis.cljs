(ns hyperfiddle.service.node.local-basis
  (:require [cats.core :refer [mlet return]]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync!]]
            [hyperfiddle.appval.runtime-local :refer [fetch-domain!]]
            [hyperfiddle.appval.runtime-rpc :refer [global-basis!]]
            [hyperfiddle.service.node.lib :as lib :refer [req->service-uri]]
            [promesa.core :as p]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.compile.reader :as reader]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.transit :as transit]

            [hyperfiddle.ide]
            [hyperfiddle.appval.domain.foundation :as foundation]
            [hyperfiddle.appval.state.reducers :as reducers]
            ))


; Todo this is same runtime as HydrateRoute
(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri foo ide-repo state-atom]
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

  ; Happened on client, node reconstructed the right rt already
  ;hyperfiddle.ide/SplitRuntime
  ;(sub-rt [rt foo ide-repo]
  ;  (LocalBasisRuntime. hyperfiddle-hostname hostname service-uri foo ide-repo state-atom))

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-local-basis [env req res path-params query-params]
  {:pre [(:ide-repo path-params) #_"Did the client rt send this with the http request?"]}
  ; Never called.
  (let [hostname (.-hostname req)
        state-val (-> {:encoded-route (base-64-url-safe/decode (:double-encoded-route path-params))
                       :global-basis (-> (:global-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
                       :user-profile (lib/req->user-profile env req)}
                      (reducers/root-reducer nil))
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (LocalBasisRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) foo (:ide-repo path-params) (reactive/atom state-val))]
    (-> (runtime/local-basis rt (:global-basis state-val) (:encoded-route state-val) branch)
        (p/then (fn [local-basis]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=31536000")
                    (.format #js {"application/transit+json" #(.send res (transit/encode local-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
