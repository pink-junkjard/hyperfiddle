(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.io.util :refer [process-result]]))


(defn hydrate-val [ptm stage-val request]
  (let [request' [(branch/branch-vals-for-request request stage-val) request]]
    (if (contains? ptm request')
      (process-result (get ptm request') request)
      (either/left {:message "Loading" :data {:request request}}))))

; react on the answer, not the question
; todo this signature should be [partition-cursor request]
(defn trackable-hydrate [state-atom branch request]
  (let [ptm @(reactive/cursor state-atom [:hyperfiddle.runtime/partitions branch :ptm])
        stage-val @(reactive/cursor state-atom [:stage])]
    (hydrate-val ptm stage-val request)))

(defn hydrate [state-atom branch request]
  (reactive/track trackable-hydrate state-atom branch request))

(defn db-pointer [uri ?branch-name]
  {:pre [uri]}
  (->DbVal uri ?branch-name))
