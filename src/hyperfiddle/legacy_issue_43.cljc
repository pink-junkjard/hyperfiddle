(ns hyperfiddle.legacy-issue-43
  (:require [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


; careful here -
; (seq [1]) - truthy
; (seq? [1]) - false
; (seq 1) - IllegalArgumentException

(defn normalize-params [porps]
  (cond (:entity porps) [(:entity porps) (:attribute porps)] ; legacy
        (instance? ThinEntity porps) [porps]                ; entity is also a map so do this first
        (map? porps) (vec (vals porps))                     ; legacy
        (coll? porps) porps
        (nil? porps) nil
        :else-scalar [porps]))
