(ns hypercrud.js.type-util)

(defn string->int
  ([s]
   (string->int s nil))
  ([s default]
   (if (and s (not-empty s))
     (let [parsed (js/parseInt s 10)]
       (if (integer? parsed) parsed default))
     default)))
