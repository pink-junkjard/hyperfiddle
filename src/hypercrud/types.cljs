(ns hypercrud.types
  (:require [cljs.reader :as reader]))


(defrecord DbVal [conn-id t])
(defrecord DbId [id conn-id])

(deftype DbHandler []
  Object
  (tag [_ v] "db")
  #_ (rep [_ v] #js [(:conn-id v) (:t v)])
  (rep [_ v] (pr-str v))
  (stringRep [this v] (pr-str v)))


(defn DbReader [v] (reader/read-string v))
