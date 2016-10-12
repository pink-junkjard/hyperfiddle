(ns hypercrud.form.q-util
  (:require [cljs.js :as cljs]))


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


(defn build-params [fill-hole query param-ctx]
  (let [q (:query/value query)]
    (->> (parse-holes q)
         (map (fn [hole-name]
                (let [value (fill-hole hole-name param-ctx)]
                  (assert (not= nil value) (str "Empty parameter for " hole-name " in " q))
                  value))))))


(defn build-params-from-formula [query param-ctx]
  (let [eval #(cljs/eval-str (cljs/empty-state) (str "(identity " % ")") nil {:eval cljs/js-eval} identity)
        hole-formulas (->> (:query/hole query)
                           (map (juxt :hole/name #(-> % :hole/formula eval)))
                           (into {}))]
    (build-params (fn [hole-name param-ctx]
                    (let [{formula :value error :error} (get hole-formulas hole-name)]
                      (if error
                        (throw error)                       ; first error, lose the rest of the errors
                        (formula param-ctx))))    ; can also throw, lose the rest
                  query param-ctx)))
