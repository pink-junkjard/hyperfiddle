(ns hyperfiddle.readers
  (:require
    [contrib.uri :refer [read-URI]]
    [hypercrud.types.DbName :refer [->DbName]]
    [hypercrud.types.ThinEntity :refer [read-ThinEntity]]))


(def dbname #(list `->DbName %))
(def entity #(list `read-ThinEntity %))
(def uri #(list `read-URI %))
