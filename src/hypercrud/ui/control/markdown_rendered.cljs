(ns hypercrud.ui.control.markdown-rendered
  (:require [hyperfiddle.core]
            [hypercrud.ui.user-attribute-renderer :refer [safe-eval-user-expr]]
            [hypercrud.ui.control.code]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.util.core :as util :refer [or-str]]
            [goog.object]
            [reagent.core :as reagent]
            [hypercrud.util.reactive :as r]))


(declare markdown)
(declare remarkInstance)

(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.control.code/code* value change! props])

; Reagent + react-context:
; https://github.com/reagent-project/reagent/commit/a8ec0d219bbd507f51a4d9276c4a1dcc020245af

(def markdown-cljs-eval
  (reagent/create-class
    {:context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [{:keys [value] :as props}]
                       (let [this (reagent/current-component)]
                         (binding [hyperfiddle.core/*ctx* (goog.object/get (.-context this) "ctx")]
                           (safe-eval-user-expr value))))}))

(def markdown-browse
  (reagent/create-class
    {:context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [{:keys [renderer ident] :as props}]
                       (let [kwargs (flatten (seq (dissoc props :value :ident)))
                             this (reagent/current-component)
                             ctx (goog.object/get (.-context this) "ctx")]
                         (apply (:browse ctx) (keyword ident) ctx kwargs)))}))

(def markdown-anchor
  (reagent/create-class
    {:context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [{:keys [label ident] :as props}]
                       (let [kwargs (flatten (seq (dissoc props :prompt :ident)))
                             this (reagent/current-component)
                             ctx (goog.object/get (.-context this) "ctx")
                             label (if-not (= label "undefined") label) ; https://github.com/medfreeman/remark-generic-extensions/issues/45
                             label (or-str label ident)]

                         (apply (:anchor ctx) (keyword ident) ctx label kwargs)))}))

(def markdown
  (reagent/create-class
    {:reagent-render
     (fn [value & [ctx]]
       (when (and (string? value) (not (empty? value)))
         (-> remarkInstance
             (.processSync value {"commonmark" true})
             .-contents)))

     :get-child-context
     (fn []
       (this-as this
         (let [[_ value ?ctx] (reagent/argv this)]
           #js {:ctx ?ctx})))

     :child-context-types
     #js {:ctx js/propTypes.object}}))

(def whitelist-reagent
  ; Div is not needed, use it with block syntax and it hits React.createElement and works
  ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
  {"span" (fn [props] [:span (dissoc props :children :value) (:value props)])
   "CodeEditor" code-editor-wrap-argv
   "block" (fn [props] [:div (dissoc props :children :value) [markdown (:value props)]])
   "cljs" markdown-cljs-eval
   "browse" markdown-browse
   "anchor" markdown-anchor})

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
                                  "block" {"html" {"properties" {"value" "::content::"}}}
                                  "cljs" {"html" {"properties" {"value" "::content::"}}}
                                  "browse" {"html" {"properties" {"renderer" "::content::" "ident" "::argument::"}}}
                                  "anchor" {"html" {"properties" {"label" "::content::" "ident" "::argument::"}}}
                                  }}))
                        (.use js/remarkGridTables)
                        (.use js/remarkReact
                              (clj->js
                                {"sanitize" false
                                 "remarkReactComponents" (->> whitelist-reagent (util/map-values reagent/reactify-component))}))))


; Todo; remove div.markdown; that should be default and style the inverse.
(defn markdown-rendered* [md & [?ctx class]]
  [:div {:class (classes "markdown" class)} [markdown md ?ctx]])

(defn markdown-relation [md ctx & class]
  ; remark creates react components which don't evaluate in this stack frame
  ; so dynamic scope is not helpful to communicate values to remark plugins
  (markdown-rendered* md ctx class))
