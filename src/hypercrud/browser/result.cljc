(ns hypercrud.browser.result
  (:require [hypercrud.browser.context :as context]
            [hypercrud.util.reactive :as reactive]))


(defn map-relation [f ctx]
  (map-indexed (fn [fe-pos fe]
                 (f (context/find-element ctx fe fe-pos)))
               (:ordered-fes ctx)))

(defn map-relations [f ctx]
  (->> (range @(reactive/map count (:relations ctx)))
       (map (fn [idx]
              (f (context/relation ctx (reactive/cursor (:relations ctx) [idx])))))))
