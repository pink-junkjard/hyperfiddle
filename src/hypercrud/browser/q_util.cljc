(ns hypercrud.browser.q-util
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.util.core :as util :refer [tee]]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [taoensso.timbre :as timbre]))


(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (as-> (util/parse-query-element q :in) elements
        (if (empty? elements) ['$] elements)
        ;; the string conversion should happen at the other side imo
        (mapv str elements)))

(defn parse-param-holes [q]
  (->> (parse-holes q)
       (remove #(string/starts-with? % "$"))))

(defn safe-parse-query-validated [fiddle]
  (mlet [q (memoized-safe-read-edn-string (:fiddle/query fiddle))]
    (if (vector? q)
      (cats/return q)
      (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))

(defn build-dbhole-lookup [ctx]
  (->> (get-in ctx [:hypercrud.browser/domain :domain/environment])
       (filter (fn [[k _]] (and (string? k) (string/starts-with? k "$"))))
       (map (juxt #(first %) (tee #(hc/db (:peer ctx) (second %) (:branch ctx))
                                  #(if (nil? (second %))
                                     (timbre/warn (second %) (keys ctx))))))
       (into {})))
