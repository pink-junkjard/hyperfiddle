(ns hyperfiddle.service.node.local-basis
  (:require [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.util.exception :refer [->Exception]]
            [hyperfiddle.api :as api]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync! transact!!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [hydrate-route! global-basis! local-basis!]]

            [hyperfiddle.service.node.lib :refer [req->service-uri]]
            [promesa.core :as p]))


(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom]
  api/AppValGlobalBasis
  (global-basis [rt]
    (global-basis! service-uri))

  api/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt hyperfiddle-hostname hostname global-basis encoded-route foo))

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

(defn http-local-basis [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (req->state-val env req path-params query-params)
        foo (some-> (:foo path-params) base-64-url-safe/decode reader/read-edn-string)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        rt (->LocalBasisRuntime (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (api/local-basis rt (:global-basis state-val) (:encoded-route state-val) foo branch)
        (p/then (fn [local-basis]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=31536000")
                    (.format #js {"application/transit+json" #(.send res (transit/encode local-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
