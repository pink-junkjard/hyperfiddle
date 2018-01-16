(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.appfn.runtime-local :refer [process-result]] ;todo
            ))


; react on the answer, not the question
(defn trackable-hydrate [state-atom request]
  (let [ptm @(reactive/cursor state-atom [:ptm])
        stage-val @(reactive/cursor state-atom [:stage])
        request' [(branch/branch-vals-for-request request stage-val) request]]
    (if (contains? ptm request')
      (process-result (get ptm request') request)
      (either/left {:message "Loading" :data {:request request}}))))

(defn hydrate [state-atom request]
  @(reactive/track trackable-hydrate state-atom request))

(defn db-pointer [state-atom uri branch-name]               ; todo remove state-atom arg
  (->DbVal uri branch-name))

(deftype Peer [state-atom]
  hypercrud/Peer
  (hydrate [this request]
    (hydrate state-atom request))

  (db [this uri branch]
    (db-pointer state-atom uri branch))

  #?@(:cljs [IHash
             (-hash [this] (goog/getUid this))]))


(defn unwrap [v']
  ; On the api side, we never inspect the error, the either is useless
  (either/branch v' (constantly nil) identity))

(deftype ApiPeer [state-atom]
  hypercrud/Peer
  (hydrate [this request]
    (unwrap                                                 ; no errors
      (hydrate state-atom request)))

  (db [this uri branch]
    (db-pointer state-atom uri branch)))
