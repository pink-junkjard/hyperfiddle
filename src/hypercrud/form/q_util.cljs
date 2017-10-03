(ns hypercrud.form.q-util
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.util.core :as util]
            [hypercrud.util.string :as hc-string]))


; deprecated
; use hypercrud.util.string/safe-read-string
(defn safe-read-string [code-str]
  (try
    (if code-str (reader/read-string code-str))             ; this doesn't handle sharp-lambdas
    (catch :default e
      ; Nothing to be done at this point -
      ; this error must be caught by the widget before it is staged.
      ;(.warn js/console "bad formula " code-str e)
      ; Happens as you type sometimes e.g. validated edn input.
      nil)))

(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (->> (util/parse-query-element q :in)
       ;; the string conversion should happen at the other side imo
       (mapv str)))

(defn parse-param-holes [q]
  (->> (parse-holes q)
       (remove #(string/starts-with? % "$"))))

(defn safe-parse-query-validated [link]
  (mlet [q (hc-string/memoized-safe-read-string (:link-query/value link))]
    (if (vector? q)
      (cats/return q)
      (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))

(defn build-dbhole-lookup [param-ctx]
  (->> (get-in param-ctx [:domain :domain/databases])
       (map (juxt :dbhole/name #(hc/db (:peer param-ctx) (:dbhole/uri %) (:branch param-ctx))))
       (into {})))

(defn form-pull-exp [form]
  (if form
    (->> (:form/field form)
         (map :field/attribute)
         (concat [:db/id])
         distinct
         (remove nil?))
    ['*]))
