(ns hypercrud.util.performance
  (:refer-clojure :exclude [time]))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  ([expr]
   `(time "Elapsed time" expr))
  ([label expr]
   `(let [start# (cljs.core/system-time)
          ret# ~expr]
      (.log js/console (str ~label ": " (.toFixed (- (cljs.core/system-time) start#) 3) " msecs"))
      ret#)))
