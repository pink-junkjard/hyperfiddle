(ns hyperfiddle.api                                         ; cljs can always import this
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [taoensso.timbre :as timbre]))


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

(defmulti tx (fn [ctx eav props]
               (let [dispatch-v (context/link-tx ctx)]
                 ; UX - users actually want to see this in console
                 (timbre/info "hf/tx: " dispatch-v " eav: " (pr-str eav))
                 dispatch-v)))

; Dispatch is a set
(defmulti render (fn [ctx props]
                   (render-dispatch ctx props)))

(defn extract-set [ctx & fs]
  (->> ctx ((apply juxt fs)) set))

(defn render-dispatch [ctx props]
  ; Is there a method which is a subset of what we've got?
  (or
    (if (= :hypercrud.browser.browser-ui/user @(get ctx :hypercrud.ui/display-mode (r/pure :hypercrud.browser.browser-ui/user)))
      (or
        (let [d (extract-set ctx context/fiddle context/a)]
          (if (contains? (methods render) d)
            d))
        (let [d (extract-set ctx context/a)]
          (if (contains? (methods render) d)
            d))))
    ; Legacy compat - options by fiddle/renderer explicit props route to select via ref renderer
    (if (:options props)
      (extract-set (context/attr ctx) :db/valueType :db/cardinality))
    (if (context/identity? ctx) #{:db.unique/identity})
    (if-let [attr (context/attr ctx)]
      (extract-set attr :db/valueType :db/cardinality))
    (if (context/element ctx)
      (extract-set ctx context/element-type))               ; :hf/variable, :hf/aggregate, :hf/pull
    ;(contrib.datomic/parser-type (context/qfind ctx))       ; :hf/find-rel :hf/find-scalar
    ;:hf/blank
    ))


(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :zero [ctx eav props]
  [])                                                       ; hack to draw as popover

(defmethod tx :db/add [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod tx :db/retract [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod tx :db.fn/retractEntity [ctx [e a v] props]
  {:pre [v]}
  [[:db.fn/retractEntity v]])

; Compat

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))

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
