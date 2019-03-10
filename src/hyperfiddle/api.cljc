(ns hyperfiddle.api                                         ; cljs can always import this
  (:require
    [contrib.data :refer [unqualify]]
    [hypercrud.browser.context :as context]))


; (d/touch (:fiddle/_links (d/entity (db! "datomic:free://datomic:4334/root") 17592186060983)))
(defn ^:export ^:legacy tempid-detached
  "Generate tempid that has not yet been used, by inspecting the stage – a side effect!"
  ([dbname ctx]
   (context/tempid! dbname ctx))
  ([ctx]
   (context/tempid! ctx)))

(defn ^:export tempid! [ctx]
  ; This is buggy. We want Collections and Sets to inspect the stage for unique generation?
  (if (context/qfind-level? ctx)
    (context/tempid! ctx)
    (context/tempid! ctx)))               ; ???

(declare render-dispatch)

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))
(defmulti render (fn [ctx props]
                   (render-dispatch ctx props)))

(defn render-dispatch [ctx props]
  (or
    (if (contains? (methods render) (context/a ctx)) (context/a ctx)) ; user override
    (if (context/identity? ctx) :db.unique/identity)
    (if-let [attr (context/attr ctx)]
      (set ((juxt :db/valueType :db/cardinality) attr)))
    ;(if (context/a ctx) :hf/attribute)
    ;(contrib.datomic/parser-type (context/el ctx))          ; :hf/variable, :hf/aggregate, :hf/pull
    ;(contrib.datomic/parser-type (context/qfind ctx))       ; :hf/find-rel :hf/find-scalar
    ;:hf/blank
    ))

(defmethod txfn :default [_ e a v ctx]
  nil)

(defmethod txfn :zero [_ e a v ctx]
  [])                                                       ; hack to draw as popover

(defmethod txfn :db/add [_ e a v ctx]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod txfn :db/retract [_ e a v ctx]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod txfn :db.fn/retractEntity [_ _ _ v ctx]
  {:pre [v]}
  [[:db.fn/retractEntity v]])
