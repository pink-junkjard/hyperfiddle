(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [clojure.core.match :refer [match match*]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [take-to unwrap]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment fix-arity-1-with-context]]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil or-str]]
    [contrib.ui]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [contrib.ui.remark :as remark]
    [contrib.ui.safe-render :refer [user-portal]]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hypercrud.ui.error :as ui-error]
    [hyperfiddle.data :as hf]
    [hyperfiddle.eval :refer [read-eval-with-bindings]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hyper-controls :refer [hyper-select hyper-select-head hyper-label]]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.form :as form]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [safe-reagent-f eval-renderer-comp]]))


(def attr-renderer
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [attr (some-> ctx :hypercrud.browser/fat-attribute deref)
            renderer (blank->nil (:attribute/renderer attr))]
        (safe-reagent-f (ui-error/error-comp ctx) eval-renderer-comp nil renderer value)))))

(defn ^:export control "this is a function, which returns component" [ctx]
  (let [attr (some-> ctx :hypercrud.browser/fat-attribute deref)
        renderer (blank->nil (:attribute/renderer attr))]
    (cond
      (not attr) controls/string
      ;renderer (attr-renderer renderer ctx)
      :else (let [type (some-> attr :db/valueType :db/ident name keyword)
                  cardinality (some-> attr :db/cardinality :db/ident name keyword)]
              (match* [type cardinality]
                [:boolean :one] controls/boolean
                [:keyword :one] controls/keyword
                [:string :one] controls/string
                [:long :one] controls/long
                [:instant :one] controls/instant
                [:ref :one] controls/dbid                   ; nested form
                [:ref :many] (constantly [:noscript]) #_edn-many ; nested table
                [_ :one] controls/edn
                [_ :many] controls/edn-many
                )))))

(declare result)

(def hyper-control'
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (fragment (when (and (not (#{:head :body} (last (:hypercrud.browser/path ctx))))
                           (keyword? (last (:hypercrud.browser/path ctx))))
                  [(control ctx) value])
                (when (some->> (:hypercrud.browser/fields ctx) (r/fmap nil?) deref)
                  [:div [result ctx]])
                [anchors (:hypercrud.browser/path ctx)]
                [iframes (:hypercrud.browser/path ctx)]))))

(defn ^:export hyper-control "Handles labels too because we show links there."
  [ctx]
  {:post [(not (nil? %))]}
  (let [head-or-body (->> (:hypercrud.browser/path ctx)
                          (reverse)
                          (take-to (comp not #{:head :body})) ; todo head/body attr collision
                          (last))
        options? (-> (->> (links-here ctx) (map :link/rel) (into #{})) ; reactivity is terrible here
                     (contains? :options))]
    (match* [head-or-body (last (:hypercrud.browser/path ctx)) options?]
      ;[:body _ true] hyper-select
      ;[:head _ true] hyper-select-head
      [:head '* _] form/magic-new-head
      [:body '* _] form/magic-new-body
      [:head _ _] hyper-label
      [:body _ _] hyper-control')))

(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (let [[i a] [nil nil]]
    (concat
      ["hyperfiddle"
       (css-slugify (:hypercrud.browser/source-symbol ctx)) ; color
       (css-slugify (cond a "attribute" i "element" :else "naked"))
       (css-slugify (-> (:TODO-IN-HEAD ctx) (if :head :body))) ; todo this is shit
       (css-slugify i)                                      ; same info as name, but by index which is more robust
       ;(css-slugify (some-> ctx :hypercrud.browser/find-element deref :type))
       ;(css-slugify (some-> ctx :hypercrud.browser/find-element deref :name)) ; works on aggregate
       (if i (css-slugify a))                               ; see attribute-schema-human
       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/valueType :db/ident))
       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :attribute/renderer #_label/fqn->name))
       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/cardinality :db/ident))
       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/isComponent (if :component)))]
      (map css-slugify (:path ctx)))))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  [relative-path ctx ?f & [props]]                          ; Path should be optional, for disambiguation only. Naked is an error
  (let [ctx (context/focus ctx (cons :body relative-path))
        props (update props :class css (semantic-css ctx))
        data @(:hypercrud.browser/data ctx)]
    [fix-arity-1-with-context (or ?f (hyper-control ctx)) data ctx props]))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables."
  [rel relative-path ctx ?content & [props]]                ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [ctx (context/focus ctx relative-path)
        link @(r/track link/rel->link rel ctx)]
    ;(assert (not render-inline?)) -- :new-fiddle is render-inline. The nav cmp has to sort this out before this unifies.
    [(:navigate-cmp ctx) (merge (link/build-link-props link ctx) props) (or ?content (name (:link/rel link))) (:class props)]))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [rel relative-path ctx ?content & [props]]                ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [ctx (context/focus ctx relative-path)
        link @(r/track link/rel->link rel ctx)]
    ;(assert render-inline?)
    [browser/ui link
     (if ?content (assoc ctx :user-renderer ?content #_(if ?content #(apply ?content %1 %2 %3 %4 args))) ctx)
     (:class props)
     (dissoc props :class :children nil)]))

; (defmulti field ::layout)
(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx ?f & [props]]
  (let [is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     ^{:key (str relative-path)}
                                     [form/table-field hyper-control relative-path ctx ?f props])
      (let [magic-new-key (when is-magic-new #_(hash @(r/fmap keys (context/entity ctx))))] ; guard against crashes for nil cell-data
        ^{:key (str relative-path magic-new-key #_"reset magic new state")}
        [form/form-field hyper-control relative-path ctx ?f props]))))

(defn ^:export table "Semantic table"
  [form sort-fn ctx & [props]]
  (let [sort-col (r/atom nil)]
    (fn [form sort-fn ctx & [props]]
      (let [ctx (assoc ctx ::sort/sort-col sort-col)]
        [:table (update props :class css "ui-table" "unp" (semantic-css ctx))
         (let [ctx (context/focus ctx [:head])]
           (->> (form ctx props) (into [:thead])))          ; strict
         (->> (:hypercrud.browser/data ctx)
              ;(r/fmap (r/partial sort-fn sort-col ctx))
              (r/unsequence hf/relation-keyfn)              ; todo support nested tables
              (map (fn [[relation k]]
                     (->> (form (context/body ctx relation) props)
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
  [ctx & [props]]
  ; focus should probably get called here. What about the request side?
  (condp = (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/block)]
                          (fragment (hf/form field ctx props)))
    :db.cardinality/many (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/table)]
                           [table (r/partial hf/form field) hf/sort-fn ctx props])
    ; blank fiddles
    nil))

(declare markdown)

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (result ctx)
     (field [] ctx nil)]))

;[user-portal hypercrud.ui.error/error-block]
(def ^:export markdown
  (remark/remark!

    ; Content is text, or more markdown, or code
    ; Argument is semantic: a url, or a hyperfiddle ident (or a second optional content? Caption, row-renderer)

    {"li" (fn [content argument props ctx]
            [:li.p (dissoc props :children) (:children props)])

     "p" (fn [content argument props ctx]
           ; Really need a way to single here from below, to get rid of div.p
           ; So that means signalling via this :children value
           (if (::unp ctx)
             (js/reactCreateFragment #js {"_" (:children props)})
             [:div.p (dissoc props :children) (:children props)]))

     "span" (fn [content argument props ctx]
              [:span (remark/adapt-props props)
               [markdown content (assoc ctx ::unp true)]])

     ; Is this comment true?::
     ;   Div is not needed, use it with block syntax and it hits React.createElement and works
     ;   see https://github.com/medfreeman/remark-generic-extensions/issues/30

     "block" (fn [content argument props ctx]
               ; Should presence of argument trigger a figure and caption?
               [:div props [markdown content (assoc ctx ::unp true)]])

     ; This is a custom markdown extension example.
     "figure" (fn [content argument props ctx]
                [:figure.figure props
                 [markdown content ctx]
                 [:figcaption.figure-caption [markdown argument (assoc ctx ::unp true)]]])

     "pre" (fn [content argument props ctx]
             ; detect ``` legacy syntax, no props or argument
             (if-let [children (:children props)]
               ; Remark generates pre>code; deep inspect and rip out the content
               ; Don't hook :code because that is used by inline snippets
               (let [content (goog.object/getValueByKeys children 0 "props" "children" 0)
                     content (str/rtrim content "\n") #_"Remark yields an unavoidable newline that we don't want"]
                 [contrib.ui/code content #() {:read-only true}])
               [contrib.ui/code content #() props]))

     ; legacy, use ``` to generate pre
     "CodeEditor" (fn [content argument props ctx]
                    [contrib.ui/code content #() props])

     "render" (fn [content argument props ctx]
                (unwrap (read-eval-with-bindings content ctx)))

     "f" (fn [content argument props ctx]
           (let [f (unwrap (read-eval-with-bindings content))
                 v (unwrap (memoized-safe-read-edn-string argument))]
             (if f
               [fix-arity-1-with-context f v ctx props])))

     "browse" (fn [content argument props ctx]
                (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
                      rel (unwrap (memoized-safe-read-edn-string srel))
                      path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
                      path (cons :body path)                ; hack to put off migrations
                      f? (unwrap (read-eval-with-bindings content))]
                  (browse rel path ctx f? props)))

     "link" (fn [content argument props ctx]
              (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
                    rel (unwrap (memoized-safe-read-edn-string srel))
                    path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
                    path (cons :body path)                  ; hack to put off migrations
                    ; https://github.com/medfreeman/remark-generic-extensions/issues/45
                    label (or-str content (name rel))]
                (link rel path ctx label props)))

     "result" (fn [content argument props ctx]
                (let [ctx (assoc ctx ::unp true)]
                  (if-let [f (unwrap (read-eval-with-bindings content))]
                    [f ctx]
                    (result ctx (update props :class css "unp")))))
     "value" (fn [content argument props ctx]
               (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                     path (cons :body path)                 ; hack to put off migrations
                     ?f (some->> (unwrap (read-eval-with-bindings content)))]
                 (value path ctx ?f props)))

     "field" (fn [content argument props ctx]
               (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                     ?f (some->> (unwrap (read-eval-with-bindings content)))]
                 (field path ctx ?f (update props :class css "unp"))))

     "table" (letfn [(form [content ctx]
                       [[markdown content (assoc ctx ::unp true)]])]
               (fn [content argument props ctx]
                 [table (r/partial form content) hf/sort-fn ctx #_props]))

     "list" (fn [content argument props ctx]
              [:ul props
               (->> (:hypercrud.browser/data ctx)
                    (r/unsequence hf/relation-keyfn)
                    (map (fn [[relation k]]
                           ^{:key k} [:li [markdown content (context/body ctx relation)]]))
                    (doall))])}))

(def ^:export img
  (from-react-context
    (fn [{:keys [props]} value]
      [:img (merge props {:src value})])))
