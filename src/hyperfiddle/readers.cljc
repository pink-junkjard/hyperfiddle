(ns hyperfiddle.readers
  (:require
    [contrib.datomic :refer [->Schema]]
    [contrib.uri :refer [read-URI]]
    #?(:cljs [goog.math])
    [hypercrud.types.DbName :refer [->DbName]]
    [hypercrud.types.ThinEntity :refer [read-ThinEntity]])
  #?(:clj
     (:import
       [java.lang Long])))


(def dbname #(list `->DbName %))
(def entity #(list `read-ThinEntity %))
(def uri #(list `read-URI %))
(def schema #(list `->Schema %))

(defn read-goog-math-long [s]
  ; #long "65332980922449989"
  ; Wrapped in string because the tag operates on a processed platform value
  ; (so Javascript has already damaged the long)
  #?(:cljs (.fromString goog.math.Long s))
  #?(:clj (Long/parseLong s)))

(def goog-math-long #(list `read-goog-math-long %))

#?(:cljs
   (defn- impl-print ^String [^goog.math.Long o]
     (str "#long " (pr-str (.toString o)))))

#?(:cljs
   (extend-type goog.math.Long
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (impl-print o)))))
