(ns contrib.reagent
  (:require [reagent.core :as reagent]))


(defn fragment [react-key & xs]
  (js/reactCreateFragment (clj->js {react-key (map reagent/as-element xs)})))

(def ^:deprecated react-fragment fragment)
