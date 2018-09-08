(ns contrib.ui.recom-date
  (:require
    [clojure.set :refer [rename-keys]]
    [contrib.data :refer [update-existing]]
    [contrib.reactive :as r]
    [contrib.ui :refer [validated-cmp]]
    [goog.date.UtcDateTime]
    [re-com.core :as re-com]))


(let [parse-iso8601-string (fn [s]
                             (if (empty? s)
                               nil
                               (let [ms (.parse js/Date s)] ; NaN if not valid string
                                 (assert (integer? ms))
                                 (js/Date. ms))))
      to-string (fn [v] (some-> v .toISOString))]           ; letfn .toISOString throws in browser? #470
  (defn iso8601-string [props]
    [validated-cmp props parse-iso8601-string to-string contrib.ui/text]))

(let [on-change (fn [v] (.-date v))]
  (defn recom-date [props]
    ; (new goog.date.UtcDateTime(new Date())).toIsoString()
    (let [props (-> props
                    (assoc :model (some-> (:value props) (goog.date.UtcDateTime.)))
                    (update-existing :on-change r/comp on-change)
                    (update :class #(str % (if (:disabled props) " disabled")))
                    (rename-keys {:disabled :disabled?})
                    (select-keys [:class :disabled? :id :model :on-change]))]
      ; I don't think :class does anything
      (into [re-com/datepicker-dropdown] (apply concat props)))))
