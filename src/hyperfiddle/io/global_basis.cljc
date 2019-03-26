(ns hyperfiddle.io.global-basis
  (:refer-clojure :exclude [compare])
  (:require
    [cats.core :as cats]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.basis :as basis]
    [hyperfiddle.io.core :as io]
    [taoensso.timbre :as timbre]))


(defn global-basis [io domain]
  (perf/time-promise
    (->> (domain/databases domain) keys set
         (io/sync io)
         (cats/fmap (fn [user-basis] {:domain (domain/basis domain) :user user-basis})))
    (fn [err total-time]
      (timbre/debugf "global-basis failure;" "total time: %sms" total-time))
    (fn [success total-time]
      (timbre/debugf "global-basis;" "total time: %sms" total-time))))

(letfn [(zero->nil [i] (when-not (= 0 i) i))]
  (defn compare [x y]
    (cond
      (identical? x y) 0
      (nil? x) -1
      (nil? y) 1
      :else (if-let [d (zero->nil (clojure.core/compare (:domain x) (:domain y)))]
              ; compare domain first, if different, the user keys might be different which is ok
              d
              (basis/compare-uri-maps (:user x) (:user y))))))
