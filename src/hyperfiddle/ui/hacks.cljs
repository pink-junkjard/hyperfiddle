(ns hyperfiddle.ui.hacks
  (:require
    [clojure.walk :refer [prewalk]]
    [hypercrud.types.Entity :refer [Entity]]))


; [contrib.ui/code-block {} (contrib.pprint/pprint-str (hyperfiddle.ui/pull-soup->tree @fiddle) 40)]
(defn ^:export pull-soup->tree [pull]
  (prewalk (fn [v]
             (if (instance? Entity v) (into {} v) v))
           pull))
