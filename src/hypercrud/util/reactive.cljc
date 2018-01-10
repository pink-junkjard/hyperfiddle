(ns hypercrud.util.reactive
  (:refer-clojure :exclude [atom map partial])
  #?(:cljs (:require [reagent.core :as reagent])))


; reactivity is currently never needed on the jvm
; so fill their implementations with naive/unreactive implementations

(defn atom [x & rest]
  (apply #?(:clj clojure.core/atom :cljs reagent/atom) x rest))

(defn cursor [src path]
  ; todo support more than just IDeref
  #?(:clj  (delay (get-in @src path))
     :cljs (reagent/cursor src path)))

(defn partial [f & args]
  (apply #?(:clj clojure.core/partial :cljs reagent/partial) f args))

(defn track [f & args]
  ; todo support more than just IDeref
  #?(:clj  (delay (apply f args))
     :cljs (apply reagent/track f args)))

(let [trackable-f (fn [reactive f] (f (deref reactive)))]
  (defn map [f reactive]
    ; if functions could be compared with =
    ; (track (comp f deref) reactive)
    (track trackable-f reactive f)))
