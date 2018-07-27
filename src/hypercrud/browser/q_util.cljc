(ns hypercrud.browser.q-util
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [contrib.data :refer [parse-query-element]]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [hypercrud.client.core :as hc]))


(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (as-> (parse-query-element q :in) elements
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
  (->> (get-in ctx [:hypercrud.browser/domain :domain/databases])
       (map (juxt :domain.database/name #(hc/db (:peer ctx) (get-in % [:domain.database/record :database/uri]) (:branch ctx))))
       (into {})))
