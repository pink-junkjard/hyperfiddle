(ns hypercrud.types.EntityRequest)


(defrecord EntityRequest [e a dbval pull-exp])

(def read-EntityRequest #(apply ->EntityRequest %))
