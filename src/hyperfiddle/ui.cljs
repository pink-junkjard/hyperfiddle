(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [clojure.core.match :refer [match match*]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment fix-arity-1-with-context]]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil or-str]]
    [contrib.ui]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [contrib.ui.remark :as remark]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.data :as hf]
    [hyperfiddle.eval :refer [read-eval-with-bindings]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.form :as form]
    [hyperfiddle.ui.select :refer [select]]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :refer [attr-renderer]]))


(defn ^:export control "this is a function, which returns component" [ctx]
  (let [layout (-> (::layout ctx :hyperfiddle.ui.layout/block) name keyword)
        a (:hypercrud.browser/attribute ctx)
        attr (some-> ctx :hypercrud.browser/fat-attribute deref)
        display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        type (some-> attr :db/valueType :db/ident name keyword)
        cardinality (some-> attr :db/cardinality :db/ident name keyword)
        isComponent (some-> attr :db/isComponent (if :component))
        renderer (blank->nil (:attribute/renderer attr))]

    (if renderer
      (attr-renderer renderer ctx)
      (match [type cardinality]
        [:boolean :one] controls/boolean
        [:keyword :one] controls/keyword
        [:string :one] controls/string
        [:long :one] controls/long
        [:instant :one] controls/instant
        [:ref :one] controls/dbid                           ; nested form
        [:ref :many] (fn [value ctx props] [:noscript]) #_edn-many ; nested table
        [_ :one] controls/edn
        [_ :many] controls/edn-many
        ))))

(defn label [field ctx props]
  (let [help-md (semantic-docstring ctx)]
    [tooltip-thick (if help-md
                     [:div.docstring [contrib.ui/markdown help-md]])
     [:label props (:label field) (if help-md [:sup "†"])]]))

(defn ^:export hyper-control "Handles labels too because we show links there."
  [ctx]
  {:post [(not (nil? %))]}
  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        d (-> (:relation ctx) (if :body :head))             ; Be careful, data/form oversets this and must be unset at call site
        i (:fe-pos ctx)
        {:keys [type]} (if i @(:hypercrud.browser/find-element ctx))
        a (:hypercrud.browser/attribute ctx)
        rels (->> (links-here ctx) (map :link/rel) (into #{}))]

    (letfn [(select? [rels] (contains? rels :options))]
      (match [d i type a rels]

        [:body i _ a (true :<< select?)]
        (fn [value ctx props]
          (fragment (anchors :body i a ctx link/options-processor) ; Order sensitive, here be floats
                    [select value ctx props]
                    (iframes :body i a ctx link/options-processor)))

        [:head i _ a (true :<< select?)]
        (fn [field ctx props]
          (fragment (if (and (= :xray display-mode)
                             (not (:link/dependent? (rel->link :options ctx))))
                      ; Float right
                      [select nil ctx props])
                    (if i [label field ctx props])
                    (anchors :head i a ctx link/options-processor)
                    (iframes :head i a ctx link/options-processor)))

        [d _ _ '* _] (case d :head form/magic-new-head
                             :body form/magic-new-body)

        [:head i _ a _]
        (fn [field ctx props]
          (fragment (if i [label field ctx props])
                    (anchors :head i a ctx)
                    (iframes :head i a ctx)))

        [:body i (:or :aggregate :variable) _ _]
        (fn [value ctx props]
          (fragment [controls/string (str value) ctx props]
                    (if i (anchors :body i a ctx))
                    (if i (iframes :body i a ctx))))

        [:body i _ a _]
        (fn [value ctx props]
          (fragment (if a [(control ctx) value ctx props])  ; element :pull ? I am missing a permutation which this this
                    (if i (anchors :body i a ctx))
                    (if i (iframes :body i a ctx))))
        ))))

(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    ["hyperfiddle"
     (css-slugify (some-> ctx :hypercrud.browser/find-element deref :source-symbol)) ; color
     (css-slugify (cond a "attribute" i "element" :else "naked"))
     (css-slugify (-> (:relation ctx) (if :body :head)))
     (css-slugify i)                                        ; same info as name, but by index which is more robust
     (css-slugify (some-> ctx :hypercrud.browser/find-element deref :type))
     (css-slugify (some-> ctx :hypercrud.browser/find-element deref :name)) ; works on aggregate
     ;(css-slugify (some-> ctx :hypercrud.browser/find-element der  ef :entity (if :entity :scalar))) ; not helpful
     (if i (css-slugify a))                                 ; see attribute-schema-human
     (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/valueType :db/ident))
     (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :attribute/renderer #_label/fqn->name))
     (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/cardinality :db/ident))
     (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/isComponent (if :component)))]))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  [[i a] ctx ?f & [props]]                                  ; Path should be optional, for disambiguation only. Naked is an error
  (let [ctx (context/focus ctx i a)
        props (update props :class css (semantic-css ctx))]
    [(or ?f (hyper-control ctx)) @(context/value ctx) ctx props]))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables."
  [rel path ctx ?content & [props]]                         ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [link @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx path)]
    ;(assert (not render-inline?)) -- :new-fiddle is render-inline. The nav cmp has to sort this out before this unifies.
    [(:navigate-cmp ctx) (merge (link/build-link-props link ctx) props) (or ?content (name (:link/rel link))) (:class props)]))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [rel path ctx ?content & [props]]                         ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (let [link @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx path)]
    ;(assert render-inline?)
    [browser/ui link
     (if ?content (assoc ctx :user-renderer ?content #_(if ?content #(apply ?content %1 %2 %3 %4 args))) ctx)
     (:class props)
     (dissoc props :class :children nil)]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [[i a] ctx ?f & [props]]
  (if-not (and (= '* a) (= :hyperfiddle.ui.layout/table (::layout ctx)))
    (let [ctx (context/focus ctx i a)]
      ^{:key (str i a)}
      [(form/-field ctx) hyper-control ?f ctx (update props :class css (semantic-css ctx))])))

(defn ^:export table "Semantic table"
  [form sort-fn ctx & [props]]
  (let [sort-col (r/atom nil)]
    (fn [form sort-fn ctx & [props]]
      (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/table
                           ::sort/sort-col sort-col)]
        [:table (update props :class css "ui-table" "unp" (semantic-css ctx))
         (->> (form ctx props) (into [:thead]))             ; strict
         (->> (:relations ctx)
              (r/fmap (r/partial sort-fn sort-col ctx))
              (r/unsequence hf/relation-keyfn)
              (map (fn [[relation k]]
                     (->> (form (context/relation ctx relation) props)
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
  [ctx & [?f props]]
  ; with-relations probably gets called here. What about the request side?
  (cond
    ?f [?f ctx]
    (:relations ctx) [table (r/partial hf/form field) hf/sort-fn ctx props]
    (:relation ctx) (fragment (hf/form field ctx props))))

(declare markdown)

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (result ctx)
     (field [] ctx nil)]))

(def extensions
  {"li" (fn [content argument props ctx]
          [:li.p (dissoc props :children) (:children props)])

   "p" (fn [content argument props ctx]
         ; Really need a way to single here from below, to get rid of div.p
         ; So that means signalling via this :children value
         (if (::unp ctx)
           (js/reactCreateFragment #js {"_" (:children props)})
           [:div.p (dissoc props :children) (:children props)]))

   "span" (fn [content argument props ctx]
            [:span (dissoc props :children) content])

   ; Is this comment true?::
   ;   Div is not needed, use it with block syntax and it hits React.createElement and works
   ;   see https://github.com/medfreeman/remark-generic-extensions/issues/30

   "block" (fn [content argument props ctx]
             [:div props [markdown content]])

   "pre" (fn [content argument props ctx]
           ; Remark generates pre>code; deep inspect and rip out the content
           ; Don't hook :code because that is used by inline snippets
           (let [content (-> props :children (goog.object/getValueByKeys 0 "props" "children" 0)) ; get(props, kw('children'))[0].props.children[0]
                 content (str/rtrim content "\n") #_"Remark yields an unavoidable newline that we don't want"]
             ; No way to get props here from userland
             [contrib.ui/code-block {:read-only true} content #()]))

   "CodeEditor" (fn [content argument props ctx]
                  [contrib.ui/code-block props content #()])

   "cljs" (fn [content argument props ctx]
            (read-eval-with-bindings content ctx))

   "browse" (fn [content argument props ctx]
              (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
                    rel (unwrap (memoized-safe-read-edn-string srel))
                    path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
                    f? (read-eval-with-bindings content)]
                (browse rel path ctx f? props)))

   "anchor" (fn [content argument props ctx]
              (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
                    rel (unwrap (memoized-safe-read-edn-string srel))
                    path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
                    ; https://github.com/medfreeman/remark-generic-extensions/issues/45
                    label (or-str content (name rel))]
                (link rel path ctx label props)))

   "result" (fn [content argument props ctx]
              (result (assoc ctx ::unp true)
                      (read-eval-with-bindings content)
                      (update props :class css "unp")))
   "value" (fn [content argument props ctx]
             (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                   ?f (read-eval-with-bindings content)
                   ?f (if ?f (r/partial fix-arity-1-with-context ?f))]
               (value path ctx ?f props)))

   "field" (fn [content argument props ctx]
             (let [props (-> props
                             (dissoc :children)
                             (clojure.set/rename-keys {:className :class})
                             (update :class css "unp") #_"fix font size")
                   path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                   ?f (read-eval-with-bindings content)
                   ?f (if ?f (r/partial fix-arity-1-with-context ?f))]
               (hyperfiddle.ui/field path ctx ?f props)))

   "table" (letfn [(form [content ctx]
                     [[markdown content (assoc ctx ::unp true)]])]
             (fn [content argument props ctx]
               [table (r/partial form content) hf/sort-fn ctx]))

   "list" (fn [content argument props ctx]
            [:ul props
             (->> (:relations ctx)
                  (r/unsequence hf/relation-keyfn)
                  (map (fn [[relation k]]
                         ; set ::unp to suppress
                         ^{:key k} [:li [markdown content (context/relation ctx relation)]]))
                  (doall))])})

;[user-portal hypercrud.ui.error/error-block]
(def ^:export markdown (remark/remark! extensions))

(def ^:export img
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [:img (merge props {:src value})])))

(defn ui-bindings [ctx]                                     ; legacy
  (assoc ctx
    :anchor link
    :browse browse
    :field field
    :cell field
    ::value value
    :browse' hf/browse'))