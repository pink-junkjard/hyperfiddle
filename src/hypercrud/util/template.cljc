(ns hypercrud.util.template
  #?(:cljs (:require-macros [hypercrud.util.template :refer [load-resource]])
     :clj
           (:require [clojure.java.io :as io])))


#?(:clj
   (defmacro load-resource [filename]
     (-> (io/resource filename) slurp)))
