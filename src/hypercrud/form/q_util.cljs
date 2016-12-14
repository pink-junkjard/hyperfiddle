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


(defn build-dbhole-lookup [link]
  (->> (:link/dbhole link)
       (map (fn [{:keys [:dbhole/name :dbhole/value]}]
              (if-not (or (empty? name) (nil? value))
                ; transform project-id into conn-id
                [name (->DbVal (-> value .-dbid .-id) nil)])))
       (into {})))


; type p-filler = fn [link param-ctx] => vec
; type fill-hole = fn [hole-name param-ctx] => param
(defn build-params [fill-hole link param-ctx]
  (->> (some-> (:link/query link) safe-read-string)
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


; returns type fill-hole (for threading into build-params)
(defn fill-hole-from-formula [link formulas]
  (let [hole-formulas (read-eval-formulas formulas)
        dbhole-values (build-dbhole-lookup link)]
    (fn [hole-name param-ctx]
      (if-let [v (get dbhole-values hole-name)]
        v
        (run-formula (get hole-formulas hole-name) param-ctx)))))


(defn build-params-from-formula [{formulas :link/formula :as link} param-ctx]
  (build-params (fill-hole-from-formula link formulas) link param-ctx))
