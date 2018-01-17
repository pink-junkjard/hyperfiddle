(ns hypercrud.types.URI)

#?(:cljs
   (deftype URI [uri-str]
     Object (toString [_] uri-str)
     IPrintWithWriter (-pr-writer [o writer _] (-write writer (str "#uri " (pr-str uri-str))))
     IComparable (-compare [this other]
                   (and (instance? URI other)
                        (compare (.-uri-str this) (.-uri-str other))))
     IHash (-hash [this] (hash uri-str))
     IEquiv (-equiv [this other]
              (and (instance? URI other)
                   (= (.-uri-str this) (.-uri-str other))))))

#?(:clj
   (defn ->URI [uri-str] (java.net.URI. uri-str)))

#?(:clj
   (defmethod print-method java.net.URI [o ^java.io.Writer w]
     (.write w (str "#uri " (pr-str (str o))))))

#?(:clj
   (defmethod print-dup java.net.URI [o w]
     (print-method o w)))

(def read-URI #(->URI %))
