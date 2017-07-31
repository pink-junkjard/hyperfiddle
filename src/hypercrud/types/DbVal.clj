(ns hypercrud.types.DbVal
  (:import [com.cognitect.transit WriteHandler ReadHandler]
           [clojure.lang ILookup]))

(deftype DbVal [conn-id branch]
  ILookup
  (valAt [o k] (get o k nil))
  (valAt [o k not-found] (case k
                           :conn-id (.conn-id o)
                           :branch (.branch o)
                           not-found)))

(defmethod print-method DbVal [o ^java.io.Writer w]
  (.write w (str "#DbVal" (pr-str [(.conn-id o) (.branch o)]))))

(defmethod print-dup DbVal [o w]
  (print-method o w))

(def read-DbVal #(apply ->DbVal %))

(deftype DbValTransitHandler []
  WriteHandler
  (tag [_ v] "DbVal")
  (rep [_ v] [(.conn-id v) (.branch v)])
  (stringRep [_ v] nil)
  (getVerboseHandler [_] nil))


(deftype DbValTransitReader []
  ReadHandler
  (fromRep [_ v] (apply ->DbVal v)))
