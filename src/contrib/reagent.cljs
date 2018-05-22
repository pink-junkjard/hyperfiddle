(ns contrib.reagent
  (:require [reagent.core :as reagent]))


(defn fragment [react-key & xs]
  (js/reactCreateFragment (clj->js {react-key (map reagent/as-element xs)})))

(defn dress "wrap naked strings into divs" [C ?f]
  (fn [value]
    (if ?f
      (let [hiccup-template (?f value)]
        (if (vector? hiccup-template)
          hiccup-template
          [C hiccup-template])))))
