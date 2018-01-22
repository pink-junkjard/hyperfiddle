(ns hyperfiddle.service.node.global-basis
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync!]]
            [hyperfiddle.appval.runtime-local :refer [global-basis]]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hypercrud.transit :as transit]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.service.node.lib :as lib :refer [req->service-uri]]
            [promesa.core :as p]))


(deftype GlobalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom]
  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

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

(defn http-global-basis [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (-> {:user-profile (lib/req->user-profile env req)}
                      (reducers/root-reducer nil))
        rt (GlobalBasisRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (runtime/global-basis rt)
        (p/then (fn [global-basis]
                  (doto res
                    (.append "Cache-Control" "max-age=0")
                    (.status 200)
                    (.format #js {"application/transit+json" #(.send res (transit/encode global-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
