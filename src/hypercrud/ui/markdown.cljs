(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]
            [hypercrud.util.core :as util]
            [hypercrud.ui.code-editor]))


(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.code-editor/code-editor* value change! props])

(def whitelist
  {"span" (fn [props] [:span (dissoc props :children :value) (:value props)])
   "div1" (fn [props] [:div (dissoc props :children :value) (:value props)])
   "CodeEditor" code-editor-wrap-argv
   "Bambi" (fn [props] [:div [:h3.bambi "bambi"]
                        [:ul [:li [:code (pr-str props)]]]])})

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
                "div1" {"html" {"properties" {"value" "::content::"}}} ; "div" causes silent failures: https://github.com/medfreeman/remark-generic-extensions/issues/30
                "CodeEditor" {"html" {"properties" {"value" "::content::"}}}
                "Bambi" {"propsDefaultValues" {}
                         "html" {"properties" {"icon" "::content::"
                                               "tooltip" "::argument::"}}}}}))
          (.use
            js/remarkReact
            (clj->js
              {"sanitize" false
               "remarkReactComponents" (util/map-values r/reactify-component whitelist)}))
          (.processSync value {"commonmark" true})
          .-contents)]
    [:div.markdown {:class (:class props)} children]))
