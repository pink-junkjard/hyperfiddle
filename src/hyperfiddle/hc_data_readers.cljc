(ns hyperfiddle.hc_data_readers
  (:require
    #?(:cljs [contrib.uuid :refer [read-uuid]])
    [contrib.datomic :refer [indexed-schema]]
    [#?(:cljs cljs.reader :clj clojure.tools.reader.default-data-readers)]
    [hyperfiddle.readers]                                   ; important
    [hypercrud.types.DbVal :refer [read-DbVal]]
    [hypercrud.types.EntityRequest :refer [read-EntityRequest]]
    [hypercrud.types.Err :refer [read-Err]]
    [hypercrud.types.QueryRequest :refer [read-QueryRequest]]
    [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
    [contrib.uri :refer [read-URI]]))


(def hc-data-readers
  {'inst #?(:cljs cljs.reader/read-date :clj clojure.tools.reader.default-data-readers/read-instant-date)
   'uuid #?(:cljs read-uuid :clj clojure.tools.reader.default-data-readers/default-uuid-reader)
   ;'queue #?(:cljs cljs.reader/read-queue) ; queue not supported on JVM: https://dev.clojure.org/jira/browse/CLJ-976
   ;'js #?(:cljs cljs.tagged-literals/read-js) ; compiler, not reader ?
   'entity read-ThinEntity
   'schema indexed-schema
   'uri read-URI
   'hypercrud.types.DbVal.DbVal read-DbVal
   'hypercrud.types.EntityRequest.EntityRequest read-EntityRequest
   'hypercrud.types.Err.Err read-Err
   'hypercrud.types.QueryRequest.QueryRequest read-QueryRequest})