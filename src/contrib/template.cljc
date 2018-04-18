(ns contrib.template
  #?(:cljs (:require-macros [contrib.template])
     :clj
           (:require [clojure.java.io :as io])))


#?(:clj
   (defmacro load-resource [filename]
     (-> (io/resource filename) slurp)))
