(ns hypercrud.ui.markdown
  (:require [hypercrud.ui.code-editor]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.code-editor/code-editor* value change! props])

(def whitelist
  {"span" (fn [props] [:span (dissoc props :children :value) (:value props)])
   ; Div is not needed, use it with block syntax and it hits React.createElement and works
   ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
   "CodeEditor" code-editor-wrap-argv})

; https://github.com/medfreeman/remark-generic-extensions

(defn markdown [value change! & [props]]
  (let [children
        (->
          (js/remark)
          (.use
            js/remarkGenericExtensions
            (clj->js
              {"elements"
               {"span" {"html" {"properties" {"value" "::content::"}}}
                "CodeEditor" {"html" {"properties" {"value" "::content::"}}}}}))
          (.use
            js/remarkReact
            (clj->js
              {"sanitize" false
               "remarkReactComponents" (util/map-values reagent/reactify-component whitelist)}))
          (.processSync value {"commonmark" true})
          .-contents)]
    [:div.markdown {:class (:class props)} children]))
