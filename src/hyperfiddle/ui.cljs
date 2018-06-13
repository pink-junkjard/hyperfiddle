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
    [contrib.ui.remark :as remark]
    [contrib.ui.tooltip :refer [tooltip-thick]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.connection-color :refer [connection-color]]
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
  (let [layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
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

(defn ^:export hyper-control [ctx]
  {:post [(not (nil? %))]}
  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        d (-> (:relation ctx) (if :body :head))
        i (:fe-pos ctx)
        {:keys [type]} (if i @(:hypercrud.browser/find-element ctx))
        a (:hypercrud.browser/attribute ctx)
        rels (->> (links-here ctx) (map :link/rel) (into #{}))]

    (if (= a '*)
      (fn [field ctx props] [:code "magic-new"])
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

          [:head i _ a _]
          (fn [field ctx props]
            (fragment (if i [label field ctx props])
                      (anchors :head i a ctx)
                      (iframes :head i a ctx)))

          [:body i (:or :aggregate :variable) _ _]
          (fn [value ctx props]
            (fragment [controls/string (str (context/extract-focus-value ctx)) ctx props]
                      (if i (anchors :body i a ctx))
                      (if i (iframes :body i a ctx))))

          [:body i _ a _]
          (fn [value ctx props]
            (fragment (if a [(control ctx) value ctx props]) ; element :pull ? I am missing a permutation which this this
                      (if i (anchors :body i a ctx))
                      (if i (iframes :body i a ctx))))
          )))))

(comment
  [:block :head i '*] (fn [field ctx props] [:code "form head magic-new"])
  [:block :body i '*] (fn [value ctx props] [:code "form body magic-new"])
  [:table :head i '*] (fn [field ctx props] [:code "table head magic-new"])
  [:table :body i '*] (fn [value ctx props] [:code "table body magic-new"])
  )

(defn ^:export semantic-css [ctx]
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

(defn ^:export value "Naked value renderer. Does not work in tables. Use field [] if you want a naked th/td view"
  [[i a] ctx ?f & args]                                     ; Doesn't make sense in a table context bc what do you do in the header?
  (let [ctx (context/focus ctx true i a)
        props (kwargs args)
        props (merge props {:class (apply css (:class props) (semantic-css ctx))})]
    [(or ?f (hyper-control ctx)) (context/extract-focus-value ctx) ctx props]))

; define nav-cmp here, and unify browser and navcmp

(defn ^:export link [rel path ctx ?content & args]
  (let [props (kwargs args)
        {:keys [link/dependent? link/render-inline?] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? path)]
    ;(assert (not render-inline?)) -- :new-fiddle is render-inline. The nav cmp has to sort this out before this unifies.
    [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) (or ?content (name (:link/rel link))) (:class props)]))

(defn ^:export browse [rel path ctx ?content & args]
  (let [props (kwargs args)
        {:keys [link/dependent? link/render-inline?] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? path)]
    (assert render-inline?)
    (into
      [browser/ui link
       (if ?content (assoc ctx :user-renderer ?content #_(if ?content #(apply ?content %1 %2 %3 %4 args))) ctx)
       (:class props)]
      (apply concat (dissoc props :class :children nil)))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [f ctx props]                                             ; fiddle-src wants to fallback by passing nil here explicitly
  (assert @(:hypercrud.ui/display-mode ctx))
  (form/ui-block-border-wrap
    ctx (css "field" "hyperfiddle-form-cell" (:class props) #_":class is for the control, these props came from !cell{}")
    ;(if (= a '*) ^{:key :new-field} [new-field ctx])
    [(or (:label-fn props) label) (:hypercrud.browser/field ctx) (dissoc ctx :relation) props]
    [f (context/extract-focus-value ctx) ctx props]))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [f ctx props]
  (let [{:keys [hypercrud.browser/field
                hypercrud.browser/attribute]} ctx
        [i a] [(:fe-pos ctx) attribute]
        path (remove nil? [i a])]
    (if (:relation ctx)
      [:td {:class (css "field" (:class props) "truncate")
            :style {:border-color (if i (form/border-color ctx))}}
       ; todo unsafe execution of user code: control
       [f (context/extract-focus-value ctx) ctx props]]
      [:th {:class (css "field" (:class props)
                        (if (and i (sort/sortable? ctx path)) "sortable") ; hoist
                        (some-> (sort/sort-direction ctx) name)) ; hoist
            :style {:background-color (connection-color ctx)}
            :on-click (r/partial sort/toggle-sort! ctx path)}
       ; Use f as the label control also, because there is hypermedia up there
       ((or (:label-fn props) f) field ctx props)])))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [[i a] ctx ?f & args]
  (let [view (case (::layout ctx) :hyperfiddle.ui.layout/table table-field form-field)
        ctx (context/focus ctx true i a)
        props (kwargs args)
        props (merge props {:class (apply css (:class props) (semantic-css ctx))})]
    ^{:key (str i a)}
    [view (or ?f (hyper-control ctx)) ctx props]))

(defn ^:export table "Semantic table; todo all markup should be injected.
sort-fn :: (fn [col ctx v] v)"
  [form sort-fn ctx]
  (let [sort-col (r/atom nil)]
    (fn [form sort-fn ctx]
      (let [ctx (assoc ctx ::layout (::layout ctx :hyperfiddle.ui.layout/table)
                           ::sort/sort-col sort-col
                           :hyperfiddle.ui.markdown-extensions/unp true)]
        [:table.ui-table.unp
         (->> (form ctx) (into [:thead]))                   ; strict
         (->> (:relations ctx)
              (r/fmap (r/partial sort-fn sort-col ctx))
              (r/unsequence hf/relation-keyfn)
              (map (fn [[relation k]]
                     (->> (form (context/relation ctx relation))
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
  [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table (r/partial hf/form field) hf/sort-fn ctx]
    (:relation ctx) (hf/form field ctx)))

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
  (into [remark/markdown -remark-instance] args))

(def ^:export img
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [:img (merge props {:src value})])))
