(ns hypercrud.react.react-fragment
  (:require [reagent.core :as reagent]))


(defn react-fragment [react-key & xs]
  (js/reactCreateFragment (clj->js {react-key (map reagent/as-element xs)})))
