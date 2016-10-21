(ns hypercrud.types
  (:require [cljs.reader :as reader]))


(deftype DbId [id conn-id]
  ;Object (toString [this] (str "#DbId" (pr-str [id conn-id])))
  IComparable (-compare [x y] (compare (:id x) (:id y)))
  IPrintWithWriter (-pr-writer [_ writer _]
                     (-write writer (str "#DbId" (pr-str [id conn-id]))))
  IHash (-hash [this] (hash [id conn-id]))
  IEquiv (-equiv [this other] (= (hash this) (hash other))))

(def read-DbId #(apply ->DbId %))


(deftype DbVal [conn-id t]
  ;Object (toString [this] (str "#DbVal" (pr-str [conn-id t])))
  IPrintWithWriter (-pr-writer [_ writer _]
                     (-write writer (str "#DbVal" (pr-str [conn-id t]))))
  IHash (-hash [this] (hash [conn-id t]))
  IEquiv (-equiv [this other] (= (hash this) (hash other))))

(def read-DbVal #(apply ->DbVal %))


(reader/register-tag-parser! 'DbId read-DbId)
(reader/register-tag-parser! 'DbVal read-DbVal)



(deftype DbHandler []
  Object
  (tag [_ v] "db")
  #_(rep [_ v] #js [(:conn-id v) (:t v)])
  (rep [_ v] (pr-str v))
  (stringRep [this v] (pr-str v)))


(defn DbReader [v] (reader/read-string v))
