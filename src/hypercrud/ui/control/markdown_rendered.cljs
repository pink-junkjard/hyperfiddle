(ns hypercrud.ui.control.markdown-rendered
  (:require [contrib.string :refer [memoized-safe-read-edn-string]]
            [hyperfiddle.core]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.user-attribute-renderer :refer [safe-eval-user-expr]]
            [hypercrud.ui.control.code]
            [hypercrud.ui.css :refer [css-slugify classes]]
            [hypercrud.util.core :as util :refer [unwrap or-str]]
            [goog.object]
            [reagent.core :as reagent]
            [hypercrud.util.reactive :as r]))


(declare markdown)
(declare remarkInstance)

(defn code-editor-wrap-argv [{:keys [value change!] :as props}]
  [hypercrud.ui.control.code/code* value change! props])

; Reagent + react-context:
; https://github.com/reagent-project/reagent/commit/a8ec0d219bbd507f51a4d9276c4a1dcc020245af

(defn child-with-ctx [f]
  (reagent/create-class
    {:context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [props]
                       (let [ctx (goog.object/get (.-context (reagent/current-component)) "ctx")]
                         (f props ctx)))}))

(def eval
  (child-with-ctx
    (fn [{:keys [value] :as props} ctx]
      (binding [hyperfiddle.core/*ctx* ctx]
        (safe-eval-user-expr value)))))

(def browse
  (child-with-ctx
    (fn [{:keys [renderer ident] :as props} ctx]
      (let [kwargs (flatten (seq (dissoc props :value :ident)))]
        (apply (:browse ctx) (keyword ident) ctx kwargs)))))

(def anchor
  (child-with-ctx
    (fn [{:keys [label ident] :as props} ctx]
      (let [kwargs (flatten (seq (dissoc props :prompt :ident)))
            ; https://github.com/medfreeman/remark-generic-extensions/issues/45
            label (or-str (if-not (= label "undefined") label) ident)]
        (apply (:anchor ctx) (keyword ident) ctx label kwargs)))))

(def cell
  (child-with-ctx
    (fn [{:keys [path] :as props} ctx]
      (let [kwargs (flatten (seq (dissoc props :prompt :ident)))
            path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" path "]"))))]
        (apply (:cell ctx) path ctx kwargs)))))

(def table
  (child-with-ctx
    (fn [{:keys [class] :as props} ctx]
      (let [kwargs (flatten (seq (dissoc props :class)))]
        (hypercrud.ui.table/Table ctx)))))

(def md-list
  (child-with-ctx
    (letfn [(keyfn [relation] (hash (map #(or (:db/id %) %) relation)))]
      (fn [{:keys [content argument class] :as props} ctx]
        (let [argument (safe-eval-user-expr argument)]      ; (fn f [content ctx & [?class]])
          [:div {:class (classes class)}
           (->> (:relations ctx)
                (r/unsequence keyfn)
                (map (fn [[relation k]]
                       (argument k content (context/relation ctx relation) (str k))))
                (doall))])))))

(def markdown
  ; remark creates react components which don't evaluate in this stack frame
  ; so dynamic scope is not helpful to communicate values to remark plugins
  (reagent/create-class
    {:reagent-render
     (fn [value & [?ctx]]
       (when (and (string? value) (not (empty? value)))
         (-> remarkInstance (.processSync value {"commonmark" true}) .-contents)))

     :get-child-context
     (fn []
       (this-as this
         (let [[_ value ?ctx] (reagent/argv this)]
           #js {:ctx ?ctx})))

     :child-context-types #js {:ctx js/propTypes.object}}))

(def whitelist-reagent
  ; Div is not needed, use it with block syntax and it hits React.createElement and works
  ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
  {"span" (fn [props] [:span (dissoc props :children :content) (:content props)])
   "p" (fn [props] [:div (-> props (dissoc :children :content) (update :class #(classes "p" %))) (:children props)])
   "CodeEditor" code-editor-wrap-argv
   "block" (fn [props] [:div (dissoc props :children :value) [markdown (:value props)]])
   "cljs" eval
   "browse" browse
   "anchor" anchor
   "cell" cell
   "table" table
   "list" md-list
   ; relations ; table renderer with docs above and below
   ; relation ? (for setting a title and docs, and then rendering the form by default links and all. Maybe can filter the relation down to a cell path?

   })

; https://github.com/medfreeman/remark-generic-extensions
; https://github.com/zestedesavoir/zmarkdown/tree/master/packages/remark-grid-tables
; https://github.com/zestedesavoir/zmarkdown/tree/master/packages/remark-custom-blocks

(def remarkInstance (-> (js/remark)
                        ;(.use js/remarkCustomBlocks (clj->js {"some" "a"}))
                        (.use js/remarkGenericExtensions
                              (clj->js
                                {"elements"
                                 {"span" {"html" {"properties" {"content" "::content::"}}}
                                  "p" {"html" {"properties" {"content" "::content::"}}}
                                  "CodeEditor" {"html" {"properties" {"value" "::content::"}}}
                                  "block" {"html" {"properties" {"value" "::content::"}}}
                                  "cljs" {"html" {"properties" {"value" "::content::"}}}
                                  "browse" {"html" {"properties" {"renderer" "::content::" "ident" "::argument::"}}}
                                  "anchor" {"html" {"properties" {"label" "::content::" "ident" "::argument::"}}}
                                  "cell" {"html" {"properties" {"f" "::content::" "path" "::argument::"}}}
                                  "table" {"html" {"properties" {"f" "::content::"}}}
                                  "list" {"html" {"properties" {"content" "::content::" "argument" "::argument::"}}}
                                  }}))
                        (.use js/remarkGridTables)
                        (.use js/remarkReact
                              (clj->js
                                {"sanitize" false
                                 "remarkReactComponents" (->> whitelist-reagent (util/map-values reagent/reactify-component))}))))


; Todo; remove div.markdown; that should be default and style the inverse.
(defn markdown-rendered* [md & [?ctx class]]
  [:div {:class (classes "markdown" class)}
   [markdown md ?ctx]])

(defn markdown-relation [k content ctx & class]
  ^{:key k} [markdown-rendered* content ctx class])
