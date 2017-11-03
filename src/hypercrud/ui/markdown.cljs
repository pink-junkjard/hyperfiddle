(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]
            [hypercrud.util.core :as util]
            [hypercrud.ui.code-editor]))


(defn code-editor-wrap-argv [{:keys [value change! props]}]
  [hypercrud.ui.code-editor/code-editor* value change! props])

(def whitelist
  {"CodeEditor" code-editor-wrap-argv
   "Bambi" (fn [] [:h3.bambi "bambi"])})

;(def showdown
;  (delay
;    (js/ReactShowdown.Converter.
;      (clj->js {"components" (util/map-values r/reactify-component whitelist)}))))
;
;(defn markdown [value change! & [props]]
;  ; :dangerouslySetInnerHTML {:__html (.convert @showdown value)}
;  [:div.markdown {:class (:class props)} (.convert @showdown value)])


(defn markdown [value change! & [props]]
  [:div.markdown {:class (:class props)}
   (-> (js/remark)
       (.use js/remarkReactRenderer)
       (.processSync value)
       .-contents)])
