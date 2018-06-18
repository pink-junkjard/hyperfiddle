(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [clojure.core.match :refer [match match*]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil]]
    [contrib.ui]
    [contrib.ui.input :refer [keyword-input* edn-input*]]
    [contrib.ui.remark :as remark]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [cuerdas.core :as str]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.data :as hf]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.form :as form]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]
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

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [ctx class]
  (let [{:keys [:hypercrud.browser/fiddle
                #_:hypercrud.browser/result]} ctx]
    [:div {:class class}
     [:h3 (some-> @fiddle :fiddle/ident name)]
     (result ctx)
     (field [] ctx nil)]))

(defn ui-bindings [ctx]                                     ; legacy
  (assoc ctx
    :anchor link                                            ; legacy
    :browse browse
    :field field
    :cell field                                             ; legacy
    ::value value
    :browse' hf/browse'))

(def -remark-instance (remark/remark
                        (reduce-kv (fn [acc k v]
                                     (assoc acc k (remark/extension k v)))
                                   (empty extensions)
                                   extensions)))

(defn ^:export markdown [& args]
  ;[user-portal hypercrud.ui.error/error-block]
  (into [remark/markdown -remark-instance] args))

(def ^:export img
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [:img (merge props {:src value})])))
