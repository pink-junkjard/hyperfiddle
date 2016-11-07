(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [hypercrud.compile.eval :as eval]
            [hypercrud.types :refer [->DbVal]]))


(defn parse-holes [q]
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= :in (first %)))
         first
         (drop 1)
         (map str))))


(defn build-dbhole-lookup [query]
  (->> (:query/dbhole query)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              ; transform project-id into conn-id
              [name (->DbVal (-> value .-dbid .-id) nil)]))
       (into {})))


; type p-filler = fn [query formulas param-ctx] => vec
; which is why we need this unused formulas param
(defn build-params [fill-hole query param-ctx]
  (let [q (reader/read-string (:query/value query))]
    (->> (parse-holes q)
         (mapv (fn [hole-name]
                 (let [value (fill-hole hole-name param-ctx)]
                   ;; Are you at the root? Can't fill holes without a project client
                   (assert (not= nil value) (str "Empty parameter for " hole-name " in " q))
                   value))))))


(defn fill-hole-from-formula [query formulas]
  (let [hole-formulas (if (empty? formulas)
                        {}
                        (->> formulas
                             reader/read-string
                             (map (juxt first #(eval/uate (str "(identity " (second %) ")"))))
                             (into {})))
        dbhole-values (build-dbhole-lookup query)]
    (fn [hole-name param-ctx]
      (if-let [v (get dbhole-values hole-name)]
        v
        (let [{formula :value error :error} (get hole-formulas hole-name)]
          (if error
            ; first error, lose the rest of the errors
            (throw error)

            ; can also throw, lose the rest
            (formula param-ctx)))))))


(defn build-params-from-formula [query formulas param-ctx]
  (build-params (fill-hole-from-formula query formulas) query param-ctx))
