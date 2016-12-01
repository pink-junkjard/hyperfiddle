(ns hypercrud.form.q-util
  (:require [cljs.reader :as reader]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.util :as util]))


(defn safe-read-string [code-str]
  (try
    (reader/read-string code-str)
    (catch js/Error e
      ;; Nothing to be done at this point -
      ;; this error must be caught by the widget before it is in the graph.
      nil)))


(defn parse-holes [q]
  (->> (util/parse-query-element q :in)
       ;; the string conversion should happen at the other side imo
       (map str)))


(defn build-dbhole-lookup [query]
  (->> (:query/dbhole query)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (->DbVal (-> value .-dbid .-id) nil)])))
       (into {})))


; type p-filler = fn [query formulas param-ctx] => vec
; which is why we need this unused formulas param
(defn build-params [fill-hole query param-ctx]
  (->> (some-> (:query/value query) safe-read-string)
       (parse-holes)                                        ; nil means '() and does the right thing
       (mapv (juxt identity #(fill-hole % param-ctx)))
       (into {})))


(defn read-eval-formulas [formulas]
  (->> (if-not (empty? formulas) (safe-read-string formulas))
       (util/map-values eval)))


(defn run-formula [{formula :value error :error} param-ctx]
  (if error
    (throw error)                                           ; first error, lose the rest of the errors
    (if formula (formula param-ctx))                        ; can also throw, lose the rest
    ))


(defn build-params-map [{formulas :link/formula :as link} param-ctx]
  (->> (read-eval-formulas formulas)
       (util/map-values #(run-formula % param-ctx))))


(defn fill-hole-from-formula [query formulas]
  (let [hole-formulas (read-eval-formulas formulas)
        dbhole-values (build-dbhole-lookup query)]
    (fn [hole-name param-ctx]
      (if-let [v (get dbhole-values hole-name)]
        v
        (run-formula (get hole-formulas hole-name) param-ctx)))))


(defn build-params-from-formula
  ([query formulas param-ctx] (build-params (fill-hole-from-formula query formulas) query param-ctx))
  ([{:keys [:link/formula :link/query] :as link} param-ctx]
   (build-params-from-formula query formula param-ctx)))
