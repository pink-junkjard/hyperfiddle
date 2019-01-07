(ns hyperfiddle.api
  (:require
    [clojure.spec.alpha :as s]
    [contrib.reactive :as r]
    [contrib.validation]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.browser.context])
  #?(:clj
     (:import
       (hypercrud.types.ThinEntity ThinEntity)
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


; This file is coded in reactive style, which means no closures because they cause unstable references.
; All "closures" must be explicitly closed with a deftype that implements IEquiv, see helpers in contrib.reactive


(defn ^:export fiddle [ctx]                                 ; remove
  (hypercrud.browser.context/fiddle ctx))

(defn ^:export spread-fiddle "automatically guards :fiddle/type :blank.
  Use in a for comprehension!"
  [ctx]
  (let [fiddle-type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))]
    (condp some [fiddle-type]
      #{:blank} []                                          ; don't we want the eav?
      #{:query :entity} [(hypercrud.browser.context/fiddle ctx)])))

(defn ^:export spread-rows "spread across resultset row-or-rows.
  Automatically accounts for query dimension - no-op in the case of FindTuple and FindScalar."
  [ctx & [sort-fn]]
  {:pre [(:hypercrud.browser/qfind ctx)
         (s/assert :hypercrud/context ctx)
         #_(not (:hypercrud.browser/element ctx))]}           ; not yet, except in recursive case
  (let [{r-qfind :hypercrud.browser/qfind r-data :hypercrud.browser/data} ctx]
    (condp some [(type @r-qfind)]
      #{FindRel FindColl} (for [[_ k] (->> (r/fmap (or sort-fn identity) r-data)
                                           (r/unsequence (r/partial hypercrud.browser.context/row-keyfn ctx)))]
                            (hypercrud.browser.context/row ctx k))
      #{FindTuple FindScalar} [ctx])))

(defn element [ctx & [i]]
  ;{:pre [(s/assert nil? (:hypercrud.browser/element ctx))]}
  #_{:post [(s/assert :hypercrud/context %)]}
  (hypercrud.browser.context/element ctx i))

(defn ^:export spread-elements "yields a ctx foreach element.
  All query dimensions have at least one element."
  [ctx]
  {:pre [(:hypercrud.browser/qfind ctx)
         (not (:hypercrud.browser/element ctx))]
   #_#_:post [(s/assert :hypercrud/context %)]}
  (let [r-qfind (:hypercrud.browser/qfind ctx)]
    ; No unsequence here? What if find elements change? Can we use something other than (range) as keyfn?
    (for [i (range (count (datascript.parser/find-elements @r-qfind)))]
      (element ctx i))))

(defn attribute [ctx a]
  {:pre [ctx a]}
  (hypercrud.browser.context/attribute ctx a))

(defn spread-pull [ctx]                                     ; not recursive
  (for [k (contrib.datomic/pull-level @(:hypercrud.browser/enclosing-pull-shape ctx))]
    (attribute ctx k)))

(defn spread-element [{:keys [:hypercrud.browser/qfind
                              :hypercrud.browser/element] :as ctx}]
  {:pre [element]}
  ; Legacy is to generate paths for the fields, field calls focus. Is this focus?
  ; If so how do we control order?
  ; focus = attribute

  ; Set EAV?
  (condp = (type element)
    Variable [ctx]
    Aggregate [ctx]
    Pull (for [k (contrib.datomic/pull-level @(:hypercrud.browser/enclosing-pull-shape ctx))]
           (attribute ctx k))))

(defn ^:export ^:legacy tempid-child
  "Generate tempid from eav, this tempid is idempotent and stable over time"
  [ctx val]
  (hypercrud.browser.context/tempid ctx))

(defn ^:export ^:legacy tempid-detached
  "Generate tempid that has not yet been used, by inspecting the stage – a side effect!"
  ([dbname ctx]
   (hypercrud.browser.context/tempid! dbname ctx))
  ([ctx]
   (hypercrud.browser.context/tempid! ctx)))

(defn ^:export tempid! [{r-eav :hypercrud.browser/eav :as ctx}]
  (if @(r/fmap-> r-eav (get 1))                             ; TODO and not a find-element, which will soon be the case. Currently broken for find elements https://github.com/hyperfiddle/hyperfiddle/issues/826
    (hypercrud.browser.context/tempid ctx)
    (hypercrud.browser.context/tempid! ctx)))

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))

(defmethod txfn :default [_ e a v ctx]
  nil)

(defmethod txfn :zero [_ e a v ctx]
  [])                                                       ; hack to draw as popover

(defmethod txfn :db/add [_ e a v ctx]
  [[:db/add e a v]])

(defmethod txfn :db/retract [_ e a v ctx]
  [[:db/retract e a v]])

(defmethod txfn :db.fn/retractEntity [_ _ _ v ctx]
  [[:db.fn/retractEntity v]])
