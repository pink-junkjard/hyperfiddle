(ns hyperfiddle.ui
  (:require
    [clojure.core.match :refer [match match*]]
    [contrib.css :refer [classes css-slugify]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [contrib.string :refer [memoized-safe-read-edn-string blank->nil]]
    [contrib.ui.remark :as remark]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.attribute.edn :refer [edn edn-many]]
    [hypercrud.ui.attribute.instant :refer [instant]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hypercrud.ui.label :refer [label]]
    [hypercrud.ui.form :as form]
    [hypercrud.ui.safe-render :refer [portal-markup]]
    [hypercrud.ui.table :as table]
    [hypercrud.ui.util :refer [attr-renderer]]
    [hyperfiddle.data :as hf]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]
    [hyperfiddle.ui.controls :as controls]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hyperfiddle.ui.select :as select]
    ))


(defn control "this is a function, which returns component" [ctx]
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
      (r/partial
        portal-markup
        (match [type cardinality]
          [:boolean :one] controls/boolean
          [:keyword :one] controls/keyword
          [:string :one] controls/string
          [:long :one] controls/long
          [:instant :one] instant
          [:ref :one] controls/dbid                         ; nested form
          [:ref :many] (fn [value ctx props] [:noscript]) #_edn-many ; nested table
          [_ :one] edn
          [_ :many] edn-many
          )))))

(defn semantic-css [ctx]
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]]
    [(css-slugify (some-> ctx :hypercrud.browser/find-element deref :source-symbol)) ; color
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

(defn head-select-xray [field ctx props]
  (fragment
    (if (:fe-pos ctx) (label field ctx props))
    (case (-> @(:hypercrud.ui/display-mode ctx) name keyword)
      :xray (if-not (-> (rel->link :options ctx) :link/dependent?)
              [select/select nil ctx (assoc props :disabled true)])
      nil)))

(defn hyper-control [ctx]
  {:post [(not (nil? %))]}
  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)
        layout (-> (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block) name keyword)
        d (-> (:relation ctx) (if :body :head))
        i (:fe-pos ctx)
        {:keys [type]} (if i @(:hypercrud.browser/find-element ctx))
        a (:hypercrud.browser/attribute ctx)
        rels (->> (links-here ctx) (map :link/rel) (into #{}))]

    (if (= a '*)
      (fn [field ctx props] [:code "magic-new"])
      (match [d i type a rels]

        [d i _ a (true :<< #(contains? % :options))]
        (r/partial portal-markup (case d :body select/select
                                         :head head-select-xray))

        [:head i _ a _]
        (fn [field ctx props]
          (fragment (if i [label field ctx props])
                    (anchors :head i a ctx nil)
                    (iframes :head i a ctx nil)))

        [:body i :aggregate _ _]
        (fn [value ctx props]
          (fragment [portal-markup controls/string (context/extract-focus-value ctx) ctx props]
                    (if i (anchors :body i a ctx nil))
                    (if i (iframes :body i a ctx nil))))

        [:body i _ a _]
        (fn [value ctx props]
          (fragment (if a [(control ctx) value ctx props])
                    (if i (anchors :body i a ctx nil))
                    (if i (iframes :body i a ctx nil))))

        ))))

(comment
  [:block :head i '*] (fn [field ctx props] [:code "form head magic-new"])
  [:block :body i '*] (fn [value ctx props] [:code "form body magic-new"])
  [:table :head i '*] (fn [field ctx props] [:code "table head magic-new"])
  [:table :body i '*] (fn [value ctx props] [:code "table body magic-new"])
  )

(defn ^:export value "Naked value renderer. Does not work in tables. Use field [] if you want a naked th/td view"
  [[i a] ctx ?f & args]                                     ; Doesn't make sense in a table context bc what do you do in the header?
  (let [ctx (context/focus ctx true i a)
        props (kwargs args)
        props (merge props {:class (apply classes (:class props) (semantic-css ctx))})]
    ; Todo pass correct value for entity links
    [(or ?f (hyper-control ctx)) (context/extract-focus-value ctx) ctx props]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [[i a] ctx ?f & args]
  (let [view (case (::layout ctx) :hyperfiddle.ui.layout/table table/Field form/Field)
        ctx (context/focus ctx true i a)
        props (kwargs args)
        props (merge props {:class (apply classes (:class props) (semantic-css ctx))})]
    ^{:key (str i a)}
    [view (or ?f (hyper-control ctx)) ctx props]))

(defn ^:export table "sort-fn :: (fn [col ctx v] v)" [form sort-fn ctx]
  (let [sort-col (r/atom nil)]
    (fn [form sort-fn ctx]
      (let [ctx (assoc ctx ::layout (::layout ctx :hyperfiddle.ui.layout/table)
                           ::table/sort-col sort-col
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

(defn link [rel path ctx ?content & args]
  (let [props (kwargs args)
        {:keys [link/dependent? link/render-inline?] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? path)]
    ;(assert (not render-inline?)) -- :new-fiddle is render-inline. The nav cmp has to sort this out before this unifies.
    [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) (or ?content (name (:link/rel link))) (:class props)]))

(defn browse [rel path ctx ?content & args]
  (let [props (kwargs args)
        {:keys [link/dependent? link/render-inline?] :as link} @(r/track link/rel->link rel path ctx)
        ctx (apply context/focus ctx dependent? path)]
    (assert render-inline?)
    (into
      [browser/ui link
       (if ?content (assoc ctx :user-renderer ?content #_(if ?content #(apply ?content %1 %2 %3 %4 args))) ctx)
       (:class props)]
      (apply concat (dissoc props :class :children nil)))))

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
