(ns hypercrud.types.QueryRequest)


(defrecord QueryRequest [query params])

(defn read-QueryRequest [v]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (->QueryRequest nil nil)
          v))
