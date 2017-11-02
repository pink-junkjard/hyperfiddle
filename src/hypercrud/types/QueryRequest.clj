(ns hypercrud.types.QueryRequest)


(defrecord QueryRequest [query params pull-exps])

(def read-QueryRequest #(apply ->QueryRequest %))
