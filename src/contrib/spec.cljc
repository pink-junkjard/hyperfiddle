(ns contrib.spec)

(def ^:private descriptions (atom {}))

(defn describe
  ([pred]
   (get @descriptions pred))
  ([pred description]
   (when (keyword pred)
     (swap! descriptions update pred merge description))))