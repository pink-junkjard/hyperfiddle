(ns hypercrud.ui.stale
  (:require [cats.monad.either :as either]
            [hypercrud.util.reactive :as reactive]))


(defn can-be-loading? [ctx]
  (reactive/cursor (.-state-atom (:peer ctx)) [:hydrate-id]))


(defn loading
  ([can-be-loading? either-v error success]
   [loading can-be-loading? either-v error success success])
  ([can-be-loading? either-v error success loading]
   (let [prev-v (atom nil)]
     (fn [can-be-loading? either-v error success loading]
       (either/branch either-v
                      (fn [e]
                        (if-let [v (and (= "Loading" (:message e)) @prev-v)]
                          (if @can-be-loading?
                            (loading v)
                            (do
                              (reset! prev-v e)
                              (error e)))
                          (do
                            (reset! prev-v e)
                            (error e))))
                      (fn [v]
                        (reset! prev-v v)
                        (success v)))))))
