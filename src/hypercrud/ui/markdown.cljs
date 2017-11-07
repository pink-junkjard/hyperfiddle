(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]
            [hypercrud.util.core :as util]
            [hypercrud.ui.code-editor]))


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
               "remarkReactComponents" (util/map-values r/reactify-component whitelist)}))
          (.processSync value {"commonmark" true})
          .-contents)]
    [:div.markdown {:class (:class props)} children]))
