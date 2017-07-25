(ns hypercrud.react.react-fragment
  (:require [reagent.core :as r]))

(defn react-fragment [react-key & xs] (js/reactCreateFragment (clj->js {react-key (map reagent.core/as-element xs)})))
