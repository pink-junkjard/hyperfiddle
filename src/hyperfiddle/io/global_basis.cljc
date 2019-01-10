(ns hyperfiddle.io.global-basis
  (:refer-clojure :exclude [compare])
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [taoensso.timbre :as timbre]))


(defn global-basis [io domain]
  (perf/time-promise
    (mlet [sync (->> (domain/databases domain)
                     keys
                     (into #{'hyperfiddle.domain/fiddle-database})
                     (io/sync io))]
      (cats/return                                          ; Just return the sync and reconstruct which is what in local-basis
        {:domain 0
         :user sync}))
    (fn [err get-total-time]
      (timbre/debug "global-basis failure;" "total time:" (get-total-time)))
    (fn [success get-total-time]
      (timbre/debug "global-basis;" "total time:" (get-total-time)))))

(def ERROR-MISMATCHED-DBNAMES "Bases cannot be compared; mismatched dbnames")
(def ERROR-BOTH-GREATER-AND-LESS-THAN "Bases cannot be compared; different values are > and <")

(letfn [(zero->nil [i] (when-not (= 0 i) i))
        (compare-uri-maps [x y]
          (if-not (= (keys x) (keys y))
            (throw (ex-info ERROR-MISMATCHED-DBNAMES {:x x :y y}))
            (reduce
              (fn [acc [xk xv]]
                (let [r (clojure.core/compare xv (get y xk))]
                  (cond
                    (= 0 acc) r
                    (= 0 r) acc
                    (not= acc r) (throw (ex-info ERROR-BOTH-GREATER-AND-LESS-THAN {:x x :y y}))
                    :else acc)))
              0
              x)))]
  (defn compare [x y]
    (cond
      (identical? x y) 0
      (nil? x) -1
      (nil? y) 1
      :else (if-let [d (zero->nil (clojure.core/compare (:domain x) (:domain y)))]
              ; compare domain first, if different, the user/ide keys might be different which is ok
              d
              (compare-uri-maps (:user x) (:user y))))))
