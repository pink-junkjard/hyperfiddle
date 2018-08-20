(ns hypercrud.client.peer
  (:require [cats.monad.either :as either]
            [contrib.reactive :as r]
            [hypercrud.types.DbVal :refer [->DbVal]]
            [contrib.uri :refer [is-uri?]]
            [hyperfiddle.io.util :refer [process-result]]))


(defn hydrate-val [request ptm]
  (if (contains? ptm request)
    (process-result (get ptm request) request)
    (either/left {:message "Loading" :data {:request request}})))

(defn hydrate [state-atom branch request]
  (->> (r/cursor state-atom [:hyperfiddle.runtime/partitions branch :ptm])
       (r/fmap (r/partial hydrate-val request))))

(defn db-pointer [uri ?branch-name]
  {:pre [uri (is-uri? uri)]}
  (->DbVal uri ?branch-name))
