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

(defn element [ctx & [i]]
  ;{:pre [(s/assert nil? (:hypercrud.browser/element ctx))]}
  #_{:post [(s/assert :hypercrud/context %)]}
  (hypercrud.browser.context/element ctx i))

(defn attribute [ctx a]
  {:pre [ctx a]}
  (hypercrud.browser.context/attribute ctx a))

(defn spread-pull [ctx]                                     ; not recursive
  (for [k (contrib.datomic/pull-level @(:hypercrud.browser/pull-enclosure ctx))]
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
    Pull (for [k (contrib.datomic/pull-level @(:hypercrud.browser/pull-enclosure ctx))]
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

(defn ^:export tempid! [ctx]
  (let [[_ a _] @(:hypercrud.browser/eav ctx)]
    (if (keyword? a)                                        ; not find-elements
      (hypercrud.browser.context/tempid ctx)
      (hypercrud.browser.context/tempid! ctx))))

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
