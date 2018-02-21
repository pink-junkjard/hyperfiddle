(ns hypercrud.compile.reader
  (:refer-clojure :exclude [read-string])
  (:require [#?(:clj clojure.tools.reader :cljs cljs.tools.reader) :as reader]
            [#?(:clj clojure.tools.reader.edn :cljs cljs.tools.reader.edn) :as edn-reader]
            [#?(:cljs cljs.reader :clj clojure.tools.reader.default-data-readers)] ; date, uuid
            [hypercrud.types.DbVal :refer [read-DbVal]]
            [hypercrud.types.EntityRequest :refer [read-EntityRequest]]
            [hypercrud.types.Err :refer [read-Err]]
            [hypercrud.types.QueryRequest :refer [read-QueryRequest]]
            [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
            [hypercrud.types.URI :refer [read-URI]]))

; cljs.tagged-literals is the cljs compile-time stuff, I chose
; not to call into that namespace, I don't understand the implications rn.

(def hc-data-readers
  {'inst #?(:cljs cljs.reader/read-date :clj clojure.tools.reader.default-data-readers/read-instant-date)
   'uuid #?(:cljs cljs.reader/read-uuid :clj clojure.tools.reader.default-data-readers/default-uuid-reader)
   ;'queue #?(:cljs cljs.reader/read-queue) ; queue not supported on JVM: https://dev.clojure.org/jira/browse/CLJ-976
   ;'js #?(:cljs cljs.tagged-literals/read-js) ; compiler, not reader ?
   'entity read-ThinEntity
   'uri read-URI
   'hypercrud.types.DbVal.DbVal read-DbVal
   'hypercrud.types.EntityRequest.EntityRequest read-EntityRequest
   'hypercrud.types.Err.Err read-Err
   'hypercrud.types.QueryRequest.QueryRequest read-QueryRequest})

(defn read-string
  ([s]
   {:pre [s]}
   (read-string {} s))
  ([opts s]
   {:pre [s]}
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
