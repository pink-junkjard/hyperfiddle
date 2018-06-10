(ns hyperfiddle.ui
  (:require
    [contrib.css :refer [classes css-slugify]]
    [contrib.data :refer [unwrap kwargs]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.ui.remark :as remark]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context fragment]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.core :as browser]
    [hypercrud.browser.link :as link]
    [hypercrud.ui.auto-control :refer [auto-control]]
    [hypercrud.ui.form :as form]
    [hypercrud.ui.table :as table]
    [hypercrud.ui.label :as label]
    [hyperfiddle.data :as hf]
    [hyperfiddle.ui.markdown-extensions :refer [extensions]]
    [hyperfiddle.ui.hacks]                                  ; exports
    [hypercrud.ui.control.link-controls :as link-controls]))


(defn ^:export value "Naked value renderer. Does not work in tables. Use field [] if you want a naked th/td view"
  [[i a] ctx ?f & args]                                     ; Doesn't make sense in a table context bc what do you do in the header?
  (let [view form/value #_(case (::layout ctx) :hyperfiddle.ui.layout/table table/value form/value)
        ctx (context/focus ctx true i a)
        props (kwargs args)
        class (classes (:class props)
                       (css-slugify a)
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/valueType :db/ident))
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/cardinality :db/ident)))
        props (merge props {:class class})]
    [view ?f ctx props]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [[i a] ctx ?f & args]
  (let [view (case (::layout ctx) :hyperfiddle.ui.layout/table table/Field form/Field)
        ctx (context/focus ctx true i a)
        props (kwargs args)
        class (classes (:class props)
                       ; Semantic css needs to be prefixed with - to avoid collisions.
                       (css-slugify (some-> ctx :hypercrud.browser/find-element deref :source-symbol)) ; color
                       (css-slugify (cond a "attribute" i "element" :else "naked"))
                       (css-slugify i)                      ; same info as name, but by index which is more robust
                       (css-slugify (some-> ctx :hypercrud.browser/find-element deref :type))
                       (css-slugify (some-> ctx :hypercrud.browser/find-element deref :name)) ; works on aggregate
                       ;(css-slugify (some-> ctx :hypercrud.browser/find-element deref :entity (if :entity :scalar))) ; not helpful
                       (if i (css-slugify a))               ; see attribute-schema-human
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/valueType :db/ident))
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :attribute/renderer #_label/fqn->name))
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/cardinality :db/ident))
                       (css-slugify (some-> ctx :hypercrud.browser/fat-attribute deref :db/isComponent (if :component))))
        props (merge props {:class class})]
    ^{:key (str i a)}
    [view ?f ctx props]))

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

; Does it get a border or not?
; We were inconsistent – !anchor was free, but xray mode drew borders,
; this seems appropriate though.
; So no border and get dependent from the ctx
; Links are not scoped though like values are; because we need all of them.
(defn form-link-view [?label link ctx props]                ; not embeds, those go through browse now?
  [(:navigate-cmp ctx) (merge props (link/build-link-props link ctx)) (or ?label (name (:link/rel link))) (:class props)]
  #_(link-controls/anchors ?label ctx props))

(defn table-link-view [?label ctx props]
  [(if (:relation ctx) :td.link-cell :th.link-cell)
   #_[:code (pr-str rel path ?label)]
   (link-controls/anchors ?label ctx props)
   ; Not embeds, those go through browse now?
   #_(link-controls/iframes path dependent? ctx)            ; inline entity-anchors are not yet implemented
   ])

(defn link [rel path ctx ?label & args]                     ; can return nil?
  (let [view (case (::layout ctx) :hyperfiddle.ui.layout/table table-link-view form-link-view)
        {:keys [link/dependent?] :as link} @(r/track link/rel->link rel path ctx)]
    ^{:key (pr-str [rel path])}
    ; ctx used for formulas and stuff
    [view ?label link (apply context/focus ctx dependent? path) (kwargs args)]))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"
  [ctx & [?f]]
  (cond
    ?f [?f ctx]
    (:relations ctx) [table (r/partial hf/form field) hf/sort-fn ctx]
    (:relation ctx) (hf/form field ctx)))

(defn browse [rel path ctx ?f & args]
  (let [props (kwargs args)
        {:keys [link/dependent?] :as link} @(r/track link/rel->link rel path ctx)
        ctx (-> (apply context/focus ctx dependent? path)
                (as-> ctx (if ?f (assoc ctx :user-renderer ?f #_(if ?f #(apply ?f %1 %2 %3 %4 args))) ctx)))]
    (into [browser/ui link ctx (:class props)] (apply concat (dissoc props :class :children nil)))))

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
