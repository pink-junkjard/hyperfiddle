(ns hypercrud.types.EntityRequest)


; todo fix db <-> dbval naming between server/client
(defrecord EntityRequest [e a dbval pull-exp])

(def read-EntityRequest #(apply ->EntityRequest %))
