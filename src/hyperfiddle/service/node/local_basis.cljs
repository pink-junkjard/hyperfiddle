(ns hyperfiddle.service.node.local-basis
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.compile.reader :as reader]
            [hypercrud.transit :as transit]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib]
            [hyperfiddle.state :as state]
            [promesa.core :as p]))


; Todo this is same runtime as HydrateRoute
(deftype LocalBasisRuntime [hyperfiddle-hostname hostname service-uri state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (reactive/cursor state-atom path))

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! service-uri))

  runtime/Route
  (encode-route [rt v]
    (ide/route-encode rt v))

  (decode-route [rt s]
    (ide/route-decode rt s))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

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

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-local-basis [env req res {:keys [encoded-route] :as path-params} query-params]
  {:pre [(:target-repo path-params) #_"Did the client rt send this with the http request?"]}
  ; Never called.
  (let [hostname (.-hostname req)
        global-basis (-> path-params :global-basis base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        route (routing/decode encoded-route)
        branch (some-> (:branch path-params) base-64-url-safe/decode reader/read-edn-string) ; todo this can throw
        branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode reader/read-edn-string)
        initial-state {:global-basis global-basis
                       :user-profile (lib/req->user-profile env req)
                       :branches {branch {:route route
                                          :hyperfiddle.runtime/branch-aux branch-aux}}}
        rt (->LocalBasisRuntime (:HF_HOSTNAME env) hostname (lib/req->service-uri env req)
                                (reactive/atom (reducers/root-reducer initial-state nil))
                                reducers/root-reducer)]
    (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
        (p/then #(runtime/local-basis rt global-basis route branch branch-aux))
        (p/then (fn [local-basis]
                  (doto res
                    (.status 200)
                    (.append "Cache-Control" "max-age=31536000")
                    (.format #js {"application/transit+json" #(.send res (transit/encode local-basis))}))))
        (p/catch (fn [error]
                   (doto res
                     (.status 500)
                     (.send (pr-str error))))))))
