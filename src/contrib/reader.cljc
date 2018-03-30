(ns contrib.reader
  (:refer-clojure :exclude [read-string])
  (:require [#?(:clj clojure.tools.reader :cljs cljs.tools.reader) :as reader]
            [#?(:clj clojure.tools.reader.edn :cljs cljs.tools.reader.edn) :as edn-reader]
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

(defn read-edn-string
  ([s]
   {:pre [s]}
   (read-edn-string {:eof nil} s))
  ([opts s]
   {:pre [s]}
   (-> (update opts :readers merge hc-data-readers)
       (edn-reader/read-string s))))
