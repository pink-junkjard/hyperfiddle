(ns hyperfiddle.service.node.local-basis
  (:require [cats.core :refer [mlet return]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.io.domain :refer [fetch-domain-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [promesa.core :as p]))


; Todo this is same runtime as HydrateRoute
(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri domain foo target-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! service-uri))

  runtime/Route
  (encode-route [rt v]
    (ide/route-encode domain v))

  (decode-route [rt s]
    (ide/route-decode domain s))

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
    (hydrate-requests-rpc! service-uri local-basis stage requests))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! service-uri dbs))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  ; Happened on client, node reconstructed the right rt already
  ;ide-rt/SplitRuntime
  ;(sub-rt [rt foo target-repo]
  ;  (LocalBasisRuntime. hyperfiddle-hostname hostname service-uri foo target-repo state-atom))
  ;(target-repo [rt] targe-repo)

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-local-basis [env req res {:keys [encoded-route] :as path-params} query-params]
  {:pre [(:target-repo path-params) #_"Did the client rt send this with the http request?"]}
  ; Never called.
  (let [hostname (.-hostname req)
        global-basis (-> path-params :global-basis base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        route (routing/decode encoded-route)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        state-val (-> {:global-basis global-basis
                       :user-profile (lib/req->user-profile env req)}
                      (reducers/root-reducer nil))]
    (-> (mlet [domain (fetch-domain-rpc! hostname (:HF_HOSTNAME env) (lib/req->service-uri env req) (:domain global-basis) state-val)
               :let [rt (LocalBasisRuntime. (:HF_HOSTNAME env) hostname (lib/req->service-uri env req) domain foo (:target-repo path-params) (reactive/atom state-val))]
               local-basis (runtime/local-basis rt global-basis route branch)]
          (return
            (doto res
              (.status 200)
              (.append "Cache-Control" "max-age=31536000")
              (.format #js {"application/transit+json" #(.send res (transit/encode local-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
