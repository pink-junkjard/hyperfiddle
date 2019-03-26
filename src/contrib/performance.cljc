(ns contrib.performance
  (:refer-clojure :exclude [time])
  #?(:cljs (:require-macros [contrib.performance]))
  (:require [promesa.core :as p]))


(defn total-time-fn-builder []
  #?(:clj  (let [start (System/currentTimeMillis)]
             #(- (System/currentTimeMillis) start))
     :cljs (let [start (system-time)]
             #(-> (- (system-time) start)
                  (.toFixed 0)))))

(defmacro time-promise [p error-fn success-fn]
  `(let [total-time-fn# (contrib.performance/total-time-fn-builder)]
     (-> ~p
         (p/then (fn [resp#]
                   (~success-fn resp# (total-time-fn#))
                   (p/resolved resp#)))
         (p/catch (fn [err#]
                    (~error-fn err# (total-time-fn#))
                    (throw err#))))))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [with-time expr]
  `(let [total-time-fn# (contrib.performance/total-time-fn-builder)
         ret# ~expr]
     (~with-time (total-time-fn#))
     ret#))
