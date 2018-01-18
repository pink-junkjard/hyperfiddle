(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.util.core :refer [unwrap]]
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
