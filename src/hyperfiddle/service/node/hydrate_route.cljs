(ns hyperfiddle.service.node.hydrate-route
  (:require [contrib.data :refer [unwrap]]
            [contrib.reactive :as r]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-loop request-fn-adapter]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.state :as state]))


(deftype HydrateRouteRuntime [hyperfiddle-hostname hostname service-uri state-atom root-reducer jwt]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  ;runtime/AppFnGlobalBasis
  ;(global-basis [rt]
  ;  (global-basis-rpc! service-uri jwt))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis route branch branch-aux stage] ; :: ... -> DataCache on the wire
    (let [data-cache (-> @(runtime/state rt [::runtime/partitions branch]) (select-keys [:tempid-lookups :ptm]))
          ctx {:hyperfiddle-hostname hyperfiddle-hostname
               :hostname hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (hydrate-loop rt (request-fn-adapter local-basis route stage ctx
                                           #(HydrateRouteRuntime. hyperfiddle-hostname hostname service-uri (r/atom %) root-reducer)
                                           #(foundation/api page-or-leaf route % ide/api))
                    local-basis branch stage data-cache)))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! service-uri local-basis stage requests jwt))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! service-uri dbs jwt))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this branch request]
    (unwrap @(hc/hydrate this branch request)))

  IHash
  (-hash [this] (goog/getUid this)))
