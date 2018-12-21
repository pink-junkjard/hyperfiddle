(ns hyperfiddle.readers
  (:require
    [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
    [contrib.datomic :refer [indexed-schema]]
    [contrib.uri :refer [read-URI]]))


(def entity #(list `read-ThinEntity %))
(def uri #(list `read-URI %))
(def schema #(list `indexed-schema %))
