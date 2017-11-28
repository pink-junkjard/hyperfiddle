(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.client.process-result :as process-result]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [reagent.core :as reagent]))


(defn trackable-hydrate [state-atom request]
  (let [ptm @(reagent/cursor state-atom [:ptm])]
    (if (contains? ptm request)
      (process-result/process-result (get ptm request) request)
      (either/left {:message "Loading" :data {:request request}}))))

(deftype Peer [state-atom]
  hypercrud/Peer
  (hydrate [this request]
    @(reagent/track trackable-hydrate state-atom request))

  (db [this uri branch]
    (->DbVal uri (branch/branch-val uri branch @(reagent/cursor state-atom [:stage]))))

  IHash
  (-hash [this] (goog/getUid this)))
