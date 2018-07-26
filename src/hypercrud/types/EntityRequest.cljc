(ns hypercrud.types.EntityRequest)


(defrecord EntityRequest [e db pull-exp])

(defn read-EntityRequest [v]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (->EntityRequest nil nil nil)
          v))
