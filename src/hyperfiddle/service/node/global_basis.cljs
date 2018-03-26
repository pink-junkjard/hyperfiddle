(ns hyperfiddle.service.node.global-basis
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.transit :as transit]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.io.global-basis :refer [global-basis]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [hyperfiddle.state :as state]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(deftype GlobalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (reactive/cursor state-atom path))

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

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

  ; IEquiv?

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-global-basis [env req res path-params query-params]
  (let [hostname (.-hostname req)
        initial-state {:user-profile (lib/req->user-profile env req)}
        rt (->GlobalBasisRuntime (:HF_HOSTNAME env) hostname (lib/req->service-uri env req)
                                 (reactive/atom (reducers/root-reducer initial-state nil))
                                 reducers/root-reducer)]
    (-> (runtime/global-basis rt)
        (p/then (fn [global-basis]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=0")
                    (.format #js {"application/transit+json" #(.send res (transit/encode global-basis))}))))
        (p/catch (fn [e]
                   (timbre/error e)
                   (lib/e->response res e))))))
