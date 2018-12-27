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
; All closures must be explicitly closed with r/partial

(defn ^:export fiddle "Fiddle level ctx adds the result to scope"
  [ctx]
  {:post [(:hypercrud.browser/eav %)                        ; the topfiddle ident is the attr
          (contains? % :hypercrud.browser/qfind)            ; but it can be nil for :type :blank
          (:hypercrud.browser/link-index %)]}
  (let [r-fiddle (:hypercrud.browser/fiddle ctx)
        r-data (:hypercrud.browser/data ctx)]
    (-> ctx
        (assoc
          :hypercrud.browser/qfind (r/fmap-> r-fiddle hypercrud.browser.context/parse-fiddle-data-shape :qfind)
          :hypercrud.browser/validation-hints
          (if-let [spec (s/get-spec @(r/fmap :fiddle/ident r-fiddle))]
            (contrib.validation/validate spec @r-data (partial hypercrud.browser.context/row-keyfn ctx))))
        (update :hypercrud.browser/eav (fn [r-eav]
                                         (-> [r-eav (r/fmap :fiddle/ident r-fiddle)] ; args to stable-eav'
                                             (r/sequence)   ; Reaction[List[_]]
                                             (->> (r/fmap (r/partial apply hypercrud.browser.context/stable-eav-a'))))
                                         )))))

(defn ^:export spread-fiddle "automatically guards :fiddle/type :blank.
  Use in a for comprehension!"
  [ctx]
  (let [fiddle-type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))]
    (condp some [fiddle-type]
      #{:blank} []
      #{:query :entity} [(fiddle ctx)])))

(defn ^:export spread-rows "spread across resultset row-or-rows.
  Automatically accounts for query dimension - no-op in the case of FindTuple and FindScalar."
  [ctx & [sort-fn]]
  {:pre [(:hypercrud.browser/qfind ctx)
         (:hypercrud.browser/eav ctx)                       ; can be just topfiddle
         (not (:hypercrud.browser/element ctx))]}           ; not yet
  (let [{r-qfind :hypercrud.browser/qfind r-data :hypercrud.browser/data} ctx]
    (condp some [(type @r-qfind)]
      #{FindRel FindColl} (for [[_ k] (->> (r/fmap (or sort-fn identity) r-data)
                                           (r/unsequence (r/partial hypercrud.browser.context/row-keyfn ctx)))]
                            ;(hypercrud.browser.context/row ctx _ k)
                            (hypercrud.browser.context/focus ctx [k]))
      #{FindTuple FindScalar} [ctx])))

(defn ^:export spread-elements "yields a ctx foreach element.
  All query dimensions have at least one element."
  [ctx]
  {:pre [(:hypercrud.browser/qfind ctx)
         (not (:hypercrud.browser/element ctx))]}
  (let [r-qfind (:hypercrud.browser/qfind ctx)]
    ; No unsequence here? What if find elements change? Can we use something other than (range) as keyfn?
    (for [[element i] (map vector (datascript.parser/find-elements @r-qfind) (range))]
      (let [ctx (assoc ctx :hypercrud.browser/element element)]
        (hypercrud.browser.context/focus ctx [i])))))

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
