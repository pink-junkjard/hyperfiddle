(ns hyperfiddle.service.node.local-basis
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync!]]
            [hyperfiddle.appval.runtime-local :refer [local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [global-basis!]]
            [hyperfiddle.service.node.lib :refer [req->service-uri req->state-val]]
            [promesa.core :as p]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.compile.reader :as reader]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.transit :as transit]))


(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis! service-uri))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt (partial hyperfiddle.ide/local-basis foo) hyperfiddle-hostname hostname global-basis encoded-route))

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

(defn http-local-basis [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (req->state-val env req path-params query-params)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (LocalBasisRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (runtime/local-basis rt (:global-basis state-val) (:encoded-route state-val) foo branch)
        (p/then (fn [local-basis]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=31536000")
                    (.format #js {"application/transit+json" #(.send res (transit/encode local-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
