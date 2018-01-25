(ns hyperfiddle.service.node.local-basis
  (:require [cats.core :refer [mlet return]]
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
            [hyperfiddle.io.local-basis :refer [fetch-domain!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [promesa.core :as p]))


; Todo this is same runtime as HydrateRoute
(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri foo target-repo state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! service-uri))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route branch]
    (mlet [domain (fetch-domain! rt hostname hyperfiddle-hostname global-basis)]
      (return
        (let [ctx {:hostname hostname
                   :hyperfiddle-hostname hyperfiddle-hostname
                   :branch branch
                   :peer rt}]
          (foundation/local-basis foo global-basis encoded-route domain ctx
                                  (partial ide/local-basis foo))))))

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
    (peer/db-pointer state-atom uri branch))

  ; Happened on client, node reconstructed the right rt already
  ;ide/SplitRuntime
  ;(sub-rt [rt foo target-repo]
  ;  (LocalBasisRuntime. hyperfiddle-hostname hostname service-uri foo target-repo state-atom))
  ;(target-repo [rt] targe-repo)

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-local-basis [env req res path-params query-params]
  {:pre [(:target-repo path-params) #_"Did the client rt send this with the http request?"]}
  ; Never called.
  (let [hostname (.-hostname req)
        state-val (-> {:encoded-route (base-64-url-safe/decode (:double-encoded-route path-params))
                       :global-basis (-> (:global-basis path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
                       :user-profile (lib/req->user-profile env req)}
                      (reducers/root-reducer nil))
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (LocalBasisRuntime. (:HF_HOSTNAME env) hostname (lib/req->service-uri env req) foo (:target-repo path-params) (reactive/atom state-val))]
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
