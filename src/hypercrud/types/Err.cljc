(ns hypercrud.types.Err)


(defrecord Err [msg])

(defn Err? [o] (instance? Err o))
