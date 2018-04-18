(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [contrib.reactive :as r]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [hypercrud.types.URI :refer [is-uri?]]
            [hyperfiddle.io.util :refer [process-result]]))


(defn partitioned-request
  ([partition-val stage-val request]
   (partitioned-request (:route partition-val)
                        (:hyperfiddle.runtime/branch-aux partition-val)
                        (:local-basis partition-val)
                        stage-val
                        request))
  ([route branch-aux local-basis stage-val request]
    ; todo this invalidation layer needs an overhaul with reactions
   request
   #_(let [relevant-dbvals (branch/request->dbvals request)]
     {:route route
      :hyperfiddle.runtime/branch-aux branch-aux
      :local-basis (let [lookup (into {} local-basis)]
                     (->> relevant-dbvals
                          (map (juxt :uri #(get lookup (:uri %))))
                          (into {})))
      :stage (branch/branch-vals-for-dbvals relevant-dbvals stage-val)
      :request request})))

(defn hydrate-val [{:keys [ptm] :as partition-val} stage-val request]
  (let [request' (partitioned-request partition-val stage-val request)]
    (if (contains? ptm request')
      (process-result (get ptm request') request)
      (either/left {:message "Loading" :data {:request request}}))))

; react on the answer, not the question
; todo this signature should be [partition-cursor request]
(defn trackable-hydrate [state-atom branch request]
  (let [partition @(r/cursor state-atom [:hyperfiddle.runtime/partitions branch])
        stage-val @(r/cursor state-atom [:stage])]
    (hydrate-val partition stage-val request)))

(defn hydrate [state-atom branch request]
  (r/track trackable-hydrate state-atom branch request))

(defn db-pointer [uri ?branch-name]
  {:pre [uri (is-uri? uri)]}
  (->DbVal uri ?branch-name))
