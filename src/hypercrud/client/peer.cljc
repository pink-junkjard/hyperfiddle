(ns hypercrud.client.peer
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [taoensso.timbre :as timbre]))


(defn hydrate [state-atom branch request]
  (r/fmap-> (r/cursor state-atom [:hyperfiddle.runtime/partitions branch :ptm])
            (get request (either/left {:message "Loading" :data {:request request}}))))

(defn loading? [e] (= "Loading" (:message e)))

(defn -quiet-unwrap [mv]
  (either/branch
    mv
    (fn [e]
      (when-not (loading? e)
        (timbre/warn e)))
    identity))
