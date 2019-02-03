(ns hypercrud.types.DbName)


(defrecord DbName [dbname])

(defn- impl-print [o]
  (str "#dbname" (pr-str (.-dbname o))))

#?(:cljs
   (extend-type DbName
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (impl-print o)))))

#?(:clj
   (defmethod print-method DbName [o ^java.io.Writer w]
     (.write w (impl-print o))))

#?(:clj
   (defmethod print-dup DbName [o w]
     (print-method o w)))
