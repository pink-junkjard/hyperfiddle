(ns hypercrud.ui.control.markdown-rendered
  (:require [hypercrud.ui.control.code]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]
            [hypercrud.compile.eval :as eval]))


(declare markdown)

(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.control.code/code* value change! props])

(def whitelist
  {"span" (fn [props] [:span (dissoc props :children :value) (:value props)])
   ; Div is not needed, use it with block syntax and it hits React.createElement and works
   ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
   "CodeEditor" code-editor-wrap-argv
   "block" (fn [props] [:div (dissoc props :children :value) (markdown (:value props))])})

; https://github.com/medfreeman/remark-generic-extensions
; https://github.com/zestedesavoir/zmarkdown/tree/master/packages/remark-grid-tables
; https://github.com/zestedesavoir/zmarkdown/tree/master/packages/remark-custom-blocks

(def remarkInstance (-> (js/remark)
                        ;(.use js/remarkCustomBlocks (clj->js {"some" "a"}))
                        (.use js/remarkGenericExtensions
                              (clj->js
                                {"elements"
                                 {"span" {"html" {"properties" {"value" "::content::"}}}
                                  "CodeEditor" {"html" {"properties" {"value" "::content::"}}}
                                  "block" {"html" {"properties" {"value" "::content::"}}}}}))
                        (.use js/remarkGridTables)
                        (.use js/remarkReact
                              (clj->js
                                {"sanitize" false
                                 "remarkReactComponents" (util/map-values reagent/reactify-component whitelist)}))))
(defn markdown [value]
  (if-let [value (eval/validate-user-code-str value)]
    (-> remarkInstance (.processSync value {"commonmark" true}) .-contents)))

; Todo; remove div.markdown; that should be default and style the inverse.
(defn markdown-rendered* [value]
  [:div.markdown (markdown value)])
