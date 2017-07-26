(ns hypercrud.util.monad
  (:require [cats.monad.either :as either]
            [cats.monad.exception :as exception]
            [cats.core :as cats]))


(defn exception->either [m]
  (if (exception/failure? m)
    (either/left (cats/extract m))
    (either/right (cats/extract m))))
