(ns contrib.ct
  (:require
    [cats.monad.either :as either]
    [cats.monad.maybe :as maybe]))


(defn unwrap [lf v+]
  (either/branch v+ lf identity))


(defn maybe [v]
  (if (some? v)
    (maybe/just v)
    (maybe/nothing)))
