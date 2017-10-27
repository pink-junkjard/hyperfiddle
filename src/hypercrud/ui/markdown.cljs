(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]))

(defn MyComponent [] [:h3.bambi "bambi"])

(def showdown (delay (js/ReactShowdown.Converter.
                       (clj->js {"components" {"MyComponent" (r/reactify-component MyComponent)}}))))

(defn markdown [value change! & [props]]
  ; :dangerouslySetInnerHTML {:__html (.convert @showdown value)}
  [:div.markdown {:class (:class props)} (.convert @showdown value)])
