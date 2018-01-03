(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.api.util :as api-util]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.reactive :as reactive]))


; react on the answer, not the question
(defn trackable-hydrate [state-atom request]
  (let [ptm @(reactive/cursor state-atom [:ptm])
        stage-val @(reactive/cursor state-atom [:stage])
        ; todo branch-val
        request' [(hash stage-val) request]]
    ; (branch/branch-val uri branch @(reactive/cursor state-atom [:stage]))
    (if (contains? ptm request')
      (api-util/process-result (get ptm request') request)
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
