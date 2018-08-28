(ns hypercrud.client.peer
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.uri :refer [is-uri?]]
    [hypercrud.types.DbVal :refer [->DbVal]]
    [hyperfiddle.io.util :refer [process-result]]
    [taoensso.timbre :as timbre]))


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

(defn -quiet-unwrap [mv]
  (either/branch
    mv
    (fn [e]
      (when-not (= "Loading" (:message e))
        (timbre/warn e)))
    identity))
