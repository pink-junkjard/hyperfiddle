(ns hypercrud.types.DbVal)


(defrecord DbVal [uri branch #_history?])

(defn read-DbVal [v]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (->DbVal nil nil)
          v))
