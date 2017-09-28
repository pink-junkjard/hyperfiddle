(ns hypercrud.types.DbVal
  (:import [com.cognitect.transit WriteHandler ReadHandler]
           [clojure.lang ILookup]))

(deftype DbVal [uri branch]
  ILookup
  (valAt [o k] (get o k nil))
  (valAt [o k not-found] (case k
                           :uri (.uri o)
                           :branch (.branch o)
                           not-found)))

(defmethod print-method DbVal [o ^java.io.Writer w]
  (.write w (str "#DbVal" (pr-str [(.uri o) (.branch o)]))))

(defmethod print-dup DbVal [o w]
  (print-method o w))

(def read-DbVal #(apply ->DbVal %))

(deftype DbValTransitHandler []
  WriteHandler
  (tag [_ v] "DbVal")
  (rep [_ v] [(.uri v) (.branch v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(deftype DbValTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->DbVal v)))
