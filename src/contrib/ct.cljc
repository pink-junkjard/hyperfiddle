(ns contrib.ct
  (:require
    [cats.core :refer [extract]]
    [cats.monad.either :as either :refer [left right]]
    [cats.monad.maybe :as maybe :refer [maybe? just? just nothing nothing?]]))


(defn unwrap [lf v+]
  (either/branch v+ lf identity))


(defn maybe [v]
  (if (some? v)
    (maybe/just v)
    (maybe/nothing)))

(defn maybe-branch [mv lf rf]
  {:pre [(maybe? mv)]}
  (if (nothing? mv)
    (lf)
    (rf (extract mv))))

(defn maybe->either [err mv]
  (maybe-branch mv #(left err) right))
