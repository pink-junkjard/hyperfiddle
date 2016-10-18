(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [hypercrud.compile.eval :as eval]))


(defn parse-holes [q]
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= :in (first %)))
         first
         (drop 2)
         (map str))))


; type p-filler = fn [query formulas param-ctx] => vec
; which is why we need this unused formulas param
(defn build-params [fill-hole query param-ctx]
  (let [q (:query/value query)]
    (->> (parse-holes q)
         (map (fn [hole-name]
                (let [value (fill-hole hole-name param-ctx)]
                  ;; Are you at the root? Can't fill holes without a project client
                  (assert (not= nil value) (str "Empty parameter for " hole-name " in " q))
                  value))))))


(defn fill-hole-from-formula [formulas]
  (let [hole-formulas (if (empty? formulas)
                        {}
                        (->> formulas
                             reader/read-string
                             (map (juxt first #(eval/uate (str "(identity " (second %) ")"))))
                             (into {})))]
    (fn [hole-name param-ctx]
      (let [{formula :value error :error} (get hole-formulas hole-name)]
        (if error
          ; first error, lose the rest of the errors
          (throw error)

          ; can also throw, lose the rest
          (formula param-ctx))))))


(defn build-params-from-formula [query formulas param-ctx]
  (build-params (fill-hole-from-formula formulas) query param-ctx))
