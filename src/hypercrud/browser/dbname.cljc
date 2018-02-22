(ns hypercrud.browser.dbname
  (:require [clojure.string :as string]
            [clojure.set :refer [map-invert]]))


(defn env [ctx]
  (->> (get-in ctx [:hypercrud.browser/domain :domain/environment])
       (filter (fn [[k v]]
                 (and (string? k) (string/starts-with? k "$"))))
       (map (fn [[k v]]
              [v k]))
       (into {})))

(defn uri->dbname [uri ctx]
  (get (env ctx) uri))

(defn dbname->uri [ctx dbname]
  (get (map-invert (env ctx)) dbname))
