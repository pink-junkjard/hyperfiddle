(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.client.process-result :as process-result]
            [hypercrud.util.branch :as branch]
            [reagent.core :as reagent]

            [hypercrud.types.DbVal :refer [->DbVal]]))


(deftype Peer [state-atom]
  hypercrud/Peer
  (hydrate [this request]
    (if-let [result @(reagent/cursor state-atom [:ptm request])]
      (process-result/process-result result request)
      (either/left {:message "Loading" :data {:request request}})))

  (db [this uri branch]
    (->DbVal uri (branch/branch-val uri branch @(reagent/cursor state-atom [:stage]))))

  IHash
  (-hash [this] (goog/getUid this)))
