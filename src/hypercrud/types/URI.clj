(ns hypercrud.types.URI
  (:import [java.lang Comparable]
           [clojure.lang IHashEq]))


(deftype URI [uri-str]
  Comparable (compareTo [x y] (compare (.uri-str x) (.uri-str y)))
  IHashEq (hasheq [this] (hash uri-str))
  Object (equals [this other]
           (and (instance? URI other)
                (= (.uri-str this) (.uri-str other)))))

(def read-URI ->URI)

(defmethod print-method URI [o ^java.io.Writer w]
  (.write w (str "#URI" (pr-str (.uri-str o)))))

(defmethod print-dup URI [o w]
  (print-method o w))
