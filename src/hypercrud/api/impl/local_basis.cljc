(ns hypercrud.api.impl.local-basis
  (:require [promesa.core :as p]))


(defn local-basis [rt global-basis encoded-route foo]
  (p/resolved global-basis))
