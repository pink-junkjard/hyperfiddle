(ns hypercrud.util.template
  (:require [clojure.java.io :as io]))


(defmacro load-resource [filename]
  (-> (io/resource filename) slurp))
