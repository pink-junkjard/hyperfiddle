(ns hypercrud.client.peer
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]))


(defn hydrate [state-atom branch request]
  (r/fmap-> (r/cursor state-atom [:hyperfiddle.runtime/partitions branch :ptm])
            (get request (either/left {:message "Loading" :data {:request request}}))))

(defn loading? [e] (= "Loading" (:message e)))
