(ns hyperfiddle.hc_data_readers
  (:require
    #?(:cljs [contrib.uuid :refer [read-uuid]])
    [contrib.datomic :refer [->Schema]]
    [#?(:cljs cljs.reader :clj clojure.tools.reader.default-data-readers)]
    [hyperfiddle.readers]                                   ; important
    [hypercrud.types.DbName :refer [->DbName]]
    [hypercrud.types.DbRef :refer [map->DbRef]]
    [hypercrud.types.EntityRequest :refer [map->EntityRequest]]
    [hypercrud.types.Err :refer [map->Err]]
    [hypercrud.types.QueryRequest :refer [map->QueryRequest]]
    [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
    [contrib.uri :refer [read-URI]]))


(def hc-data-readers
  {'inst #?(:cljs cljs.reader/read-date :clj clojure.tools.reader.default-data-readers/read-instant-date)
   'uuid #?(:cljs read-uuid :clj clojure.tools.reader.default-data-readers/default-uuid-reader)
   ;'queue #?(:cljs cljs.reader/read-queue) ; queue not supported on JVM: https://dev.clojure.org/jira/browse/CLJ-976
   ;'js #?(:cljs cljs.tagged-literals/read-js) ; compiler, not reader ?
   'dbname ->DbName
   'entity read-ThinEntity
   'schema ->Schema
   'uri read-URI
   'long hyperfiddle.readers/read-goog-math-long
   'hypercrud.types.DbRef.DbRef map->DbRef
   'hypercrud.types.EntityRequest.EntityRequest map->EntityRequest
   'hypercrud.types.Err.Err map->Err
   'hypercrud.types.QueryRequest.QueryRequest map->QueryRequest})