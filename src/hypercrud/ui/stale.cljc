(ns hypercrud.ui.stale
  (:require [cats.monad.either :as either]
            [hyperfiddle.runtime :as runtime]))


(defn can-be-loading? [ctx]
  (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :hydrate-id]))

(defn loading
  ([can-be-loading either-v error success]
   [loading can-be-loading either-v error success success])
  ([can-be-loading either-v error success loading]
   (let [prev-ev (atom nil)]
     (fn [can-be-loading either-v error success loading]
       (either/branch either-v
                      (fn [e]
                        (if-let [ev (and (= "Loading" (:message e)) @prev-ev)]
                          (if @can-be-loading?
                            (either/branch ev error loading)
                            (do
                              (reset! prev-ev either-v)
                              (error e)))
                          (do
                            (reset! prev-ev either-v)
                            (error e))))
                      (fn [v]
                        (reset! prev-ev either-v)
                        (success v)))))))
