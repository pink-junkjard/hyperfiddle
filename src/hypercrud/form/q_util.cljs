(ns hypercrud.form.q-util
  (:require [hypercrud.compile.eval :as eval]))


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


(defn fill-hole-from-formula [query]
  (let [hole-formulas (->> (:query/hole query)
                           (map (juxt :hole/name #(eval/uate (str "(identity " (:hole/formula %) ")"))))
                           (into {}))]
    (fn [hole-name param-ctx]
      (let [{formula :value error :error} (get hole-formulas hole-name)]
        (if error
          ; first error, lose the rest of the errors
          (throw error)

          ; can also throw, lose the rest
          (formula param-ctx))))))


(defn build-params-from-formula [query param-ctx]
  (build-params (fill-hole-from-formula query) query param-ctx))
