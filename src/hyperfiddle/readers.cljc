(ns hyperfiddle.readers
  (:require [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
            [contrib.uri :refer [read-URI]]))


(def entity #(list `read-ThinEntity %))
(def uri #(list `read-URI %))
