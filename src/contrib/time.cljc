(ns contrib.time)

; "yyyy/MM/dd"
; "MM/dd/yyyy"
#?(:clj (defn inst->date-str [fmt v]
          (.format (java.text.SimpleDateFormat. fmt) v))
   :cljs nil)