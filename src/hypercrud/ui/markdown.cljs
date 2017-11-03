(ns hypercrud.ui.markdown
  (:require [reagent.core :as r]
            [hypercrud.util.core :as util]
            [hypercrud.ui.code-editor]))


(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.code-editor/code-editor* value change! props])

(def whitelist
  {"CodeEditor" code-editor-wrap-argv
   "Bambi" (fn [props] [:div
                        [:h3.bambi "bambi"]
                        [:code (pr-str props)]])})

(defn markdown [value change! & [props]]
  (let [children
        (->
          (js/remark)
          (.use
            js/remarkGenericExtensions
            (clj->js
              {"elements" {"Bambi" {"propsDefaultValues" {}
                                    "html" {"properties" {"icon" "::content::"
                                                          "tooltip" "::argument::"}}}
                           "CodeEditor" {"html" {"properties" {"value" "::content::"}}}}}))
          (.use
            js/remarkReact
            (clj->js
              {"sanitize" false
               "remarkReactComponents" (util/map-values r/reactify-component whitelist)}))
          (.processSync value {"commonmark" true})
          .-contents)]
    [:div.markdown {:class (:class props)} children]))
