(ns hypercrud.ui.stale
  (:require [cats.monad.either :as either]))


(defn loading
  ([either-v error success]
   [loading either-v error success success])
  ([either-v error success loading]
   (let [prev-v (atom nil)]
     (fn [either-v error success loading]
       (either/branch either-v
                      (fn [e]
                        (if-let [v (and (= "Loading" (:message e)) @prev-v)]
                          (loading v)
                          (error e)))
                      (fn [v]
                        (reset! prev-v v)
                        (success v)))))))
