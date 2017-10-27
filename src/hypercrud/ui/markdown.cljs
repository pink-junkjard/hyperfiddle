(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]))

(def MyComponent (r/as-element [:pre.bambi "hello world"]))
(def showdown (delay (js/ReactShowdown.Converter. #js {"MyComponent" MyComponent})))

(defn markdown [value change! & [props]]
  ; :dangerouslySetInnerHTML {:__html (.convert @showdown value)}
  [:div.markdown {:class (:class props)} (.convert @showdown value)])
