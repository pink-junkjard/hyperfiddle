(ns hypercrud.util.reactive
  (:refer-clojure :exclude [atom partial])
  (:require [cats.core :as cats]
            [hypercrud.util.core :as util]
    #?(:cljs [reagent.core :as reagent])
    #?(:cljs [reagent.ratom :refer [IReactiveAtom]]))
  #?(:clj
     (:import (clojure.lang IAtom IDeref))))


; reactivity is currently never needed on the jvm
; so fill their implementations with naive/unreactive implementations

(defn reactive? [v]
  #?(:clj  (instance? IDeref v)
     :cljs (satisfies? IReactiveAtom v)))

(defn atom [x & rest]
  (apply #?(:clj clojure.core/atom :cljs reagent/atom) x rest))

(defn cursor [src path]
  {:pre [(reactive? src)]}
  ; todo support more than just IDeref
  #?(:clj  (delay (get-in @src path))
     :cljs (reagent/cursor src path)))

(defn partial [f & args]
  (apply #?(:clj clojure.core/partial :cljs reagent/partial) f args))

(defn track [f & args]
  ; todo support more than just IDeref
  #?(:clj  (delay (apply f args))
     :cljs (apply reagent/track f args)))

(let [trackable-f (fn [rv f] (f (deref rv)))]               ; stable ref
  (defn fmap [f rv]
    {:pre [(reactive? rv)]}
    ; (track (comp f deref) rv) -- unstable fn ref breaks optimizations
    (track trackable-f rv f)))

(letfn [(-fapply [rf rv] (@rf @rv))]
  (defn fapply [rf & rvs]
    {:pre [(reactive? rf) (every? reactive? rvs)]}
    (reduce (partial track -fapply rf) rvs)))

; Reactive[Monad[_]] => Reactive[Monad[Reactive[_]]]
; useful for reacting on the Either (left v right), but not the Right's value
; this should probably fall out eventually
(let [f (fn [rmv] (cats/fmap (constantly (fmap cats/extract rmv)) @rmv))]
  (defn apply-inner-r [rmv]
    (track f rmv)))

(defn unsequence
  "Expand a reference of a list into a list of references while maintaining order.

  If `key-fn` is provided, the children cursors will be pathed by the provided key-fn, NOT index.
  This is useful when the child cursors' references must be consistent across reorderings (which index does not provide).
  Like track's and fmap's `f`, `key-fn` MUST be stable across invocations to provide stable child references."
  ([rv]
   {:pre [(reactive? rv)]}
   (->> (range @(fmap count rv))
        (map (fn [index] [(cursor rv [index]) index]))))
  ([key-fn rv]
   {:pre [(reactive? rv)]}
   (let [lookup (fmap (partial util/group-by-unique key-fn) rv)]
     (->> @(fmap (partial map key-fn) rv)
          (map (fn [key] [(cursor lookup [key]) key]))))))
