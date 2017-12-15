(ns hypercrud.compile.reader
  (:require [#?(:clj  clojure.tools.reader
                :cljs cljs.tools.reader) :as reader]
            [#?(:clj  clojure.tools.reader.edn
                :cljs cljs.tools.reader.edn) :as edn-reader]
            [hypercrud.types.DbVal :refer [read-DbVal]]
            [hypercrud.types.EntityRequest :refer [read-EntityRequest]]
            [hypercrud.types.Err :refer [read-Err]]
            [hypercrud.types.QueryRequest :refer [read-QueryRequest]]
            [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
            [hypercrud.types.URI :refer [read-URI]]))


(def hc-data-readers
  {'entity read-ThinEntity
   'uri read-URI
   'hypercrud.types.DbVal.DbVal read-DbVal
   'hypercrud.types.EntityRequest.EntityRequest read-EntityRequest
   'hypercrud.types.Err.Err read-Err
   'hypercrud.types.QueryRequest.QueryRequest read-QueryRequest})

(defn read-string
  ([s]
   (read-string {} s))
  ([opts s]
   (binding [reader/*data-readers* (merge hc-data-readers reader/*data-readers*)]
     (reader/read-string opts s))))

(defn read-edn-string
  ([s]
   (read-edn-string {} s))
  ([opts s]
   (-> (update opts :readers merge hc-data-readers)
       (edn-reader/read-string s))))
