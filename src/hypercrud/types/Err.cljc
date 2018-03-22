(ns hypercrud.types.Err)


(defrecord Err [msg])

(defn read-Err [v]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (->Err nil)
          v))

(defn Err? [o] (instance? Err o))
