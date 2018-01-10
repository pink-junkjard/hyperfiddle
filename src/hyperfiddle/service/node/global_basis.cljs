(ns hyperfiddle.service.node.global-basis
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.api :as api]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync! transact!!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [hydrate-route! global-basis! local-basis!]]

            [hypercrud.types.URI :refer [->URI]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.service.node.lib :refer [req->service-uri]]
            [promesa.core :as p]))


(deftype GlobalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom]
  api/AppValGlobalBasis
  (global-basis [rt]
    (global-basis rt hyperfiddle-hostname hostname))

  api/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests! service-uri local-basis stage requests))

  api/AppFnSync
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
        state-val (req->state-val env req path-params query-params)
        rt (GlobalBasisRuntime. (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (api/global-basis rt)
        (p/then (fn [global-basis]
                  (doto res
                    (.append "Cache-Control" "max-age=0")
                    (.status 200)
                    (.format #js {"application/transit+json" #(.send res (transit/encode global-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
