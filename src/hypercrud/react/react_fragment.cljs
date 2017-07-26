(ns hypercrud.react.react-fragment
  (:require [reagent.core :as r]))

(defn react-fragment [xs & {k :react-key :or {k :_}}]
  (js/reactCreateFragment (clj->js {k (map r/as-element xs)})))
