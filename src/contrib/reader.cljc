(ns contrib.reader
  (:refer-clojure :exclude [read-string])
  (:require
    [contrib.try$ :refer [try-either]]
    [clojure.tools.reader :as reader]
    [clojure.tools.reader.edn :as edn-reader]
    [hyperfiddle.hc_data_readers :refer [hc-data-readers]]))

; cljs.tagged-literals is the cljs compile-time stuff, I chose
; not to call into that namespace, I don't understand the implications rn.

(defn read-string                                           ; i dont like this collision with clojure.core.read-string it is confusing
  ([s]
   {:pre [s]}
   (read-string {} s))
  ([opts s]
   {:pre [s]}
    ; Can't this be done with (set! *data-readers*) in main?
   (binding [reader/*data-readers* (merge hc-data-readers reader/*data-readers*)]
     (reader/read-string opts s))))

(defn read-edn-string!
  ([s]
   (read-edn-string! {:eof nil} s))
  ([opts s]
   (-> (update opts :readers merge hc-data-readers)
       (edn-reader/read-string s))))

(defn read-edn-string+ [& args] (try-either (apply read-edn-string! args)))

(def memoized-read-edn-string+ (memoize read-edn-string+))
