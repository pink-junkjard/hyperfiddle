(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.api.util :as api-util]
            [hypercrud.client.core :as hypercrud]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [reagent.core :as reagent]))


(defn trackable-hydrate [state-atom request]
  (let [ptm @(reagent/cursor state-atom [:ptm])]
    (if (contains? ptm request)
      (api-util/process-result (get ptm request) request)
      (either/left {:message "Loading" :data {:request request}}))))

(defn hydrate [state-atom request]
  @(reagent/track trackable-hydrate state-atom request))

(defn db [state-atom uri branch]
  (->DbVal uri (branch/branch-val uri branch @(reagent/cursor state-atom [:stage]))))

(deftype Peer [state-atom]
  hypercrud/Peer
  (hydrate [this request]
    (hydrate state-atom request))

  (db [this uri branch]
    (db state-atom uri branch))

  IHash
  (-hash [this] (goog/getUid this)))
