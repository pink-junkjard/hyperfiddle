(ns hypercrud.types.DbVal
  (:import [clojure.lang ILookup]))


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
