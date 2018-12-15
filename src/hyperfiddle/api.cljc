(ns hyperfiddle.api
  (:require
    [hypercrud.browser.context]))



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

(defn ^:export tempid! [{[e a v] :hypercrud.browser/eav
                         :as ctx}]
  (if a                                                     ; TODO and not a find-element, which will soon be the case. Currently broken for find elements https://github.com/hyperfiddle/hyperfiddle/issues/826
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
