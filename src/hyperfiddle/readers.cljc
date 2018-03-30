(ns hyperfiddle.readers
  (:require [hypercrud.types.ThinEntity :refer [read-ThinEntity]]
            [hypercrud.types.URI :refer [read-URI]]))


(def entity #(list `read-ThinEntity %))
(def uri #(list `read-URI %))
