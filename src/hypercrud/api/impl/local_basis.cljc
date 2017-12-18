(ns hypercrud.api.impl.local-basis
  (:require [promesa.core :as p]))


(defn local-basis [rt global-basis]
  (p/resolved global-basis))
