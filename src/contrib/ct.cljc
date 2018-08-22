(ns contrib.ct
  (:require
    [cats.monad.either :as either]))


(defn unwrap [lf v+]
  (either/branch v+ lf identity))
