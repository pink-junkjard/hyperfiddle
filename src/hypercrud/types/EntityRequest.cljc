(ns hypercrud.types.EntityRequest)


(defrecord EntityRequest [e a db pull-exp])

(defn read-EntityRequest [v]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (->EntityRequest nil nil nil nil)
          v))
