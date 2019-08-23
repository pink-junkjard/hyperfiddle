(ns hyperfiddle.ui.stale
  (:require
    [cats.monad.either :as either]))


(defn- loading? [e] (= "Loading" (:message e)))

(defn loading
  ([can-be-loading either-v error success]
   [loading can-be-loading either-v error success success])
  ([can-be-loading either-v error success loading]
   (let [prev-ev (atom nil)]
     (fn [can-be-loading either-v error success loading]
       (either/branch either-v
                      (fn [e]
                        (if-let [ev (and (loading? e) @prev-ev)]
                          (if @can-be-loading
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
