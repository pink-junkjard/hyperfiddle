(ns hypercrud.types.URI)


(defn read-URI [v] (java.net.URI. v))

(defmethod print-method java.net.URI [o ^java.io.Writer w]
  (.write w (str "#URI " (pr-str (str o)))))

(defmethod print-dup java.net.URI [o w]
  (print-method o w))
