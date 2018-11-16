(ns hyperfiddle.api
  (:require
    [hyperfiddle.tempid :refer [with-tempid-color tempid-from-ctx tempid-from-stage]]))


(defn ^:export tempid-child "stable and idempotent, but implemented through parent-child ctx"
  [ctx val]
  (with-tempid-color ctx tempid-from-ctx))

; This returns a new value each time the transaction changes - can't call it again later.
; So tx-fns must inspect the modal-route, they can't re-create the dbid.
(defn ^:export tempid-detached "unstable but guaranteed unique tempid"
  ([dbname ctx]
   (with-tempid-color dbname ctx (partial tempid-from-stage dbname)))
  ([ctx]
   (with-tempid-color ctx tempid-from-stage)))

(defmulti txfn identity)

(defmethod txfn :default [_] nil)
(defmethod txfn :zero [_] (constantly {:tx []}))            ; hack to draw as popover

(defmethod txfn :db/add [_]
  (fn [ctx multi-color-tx modal-route]
    (let [[_ [{entity-id :db/id}]] modal-route
          parent-id (-> ctx :hypercrud.browser/parent hypercrud.browser.context/id)
          attr (last (:hypercrud.browser/path ctx))]
      {:tx {(hypercrud.browser.context/uri ctx)
            [[:db/add parent-id attr entity-id]]}})))

(defmethod txfn :db.fn/retractEntity [_]
  (fn [ctx multi-color-tx modal-route]
    {:tx {(hypercrud.browser.context/uri ctx)
          [[:db.fn/retractEntity (hypercrud.browser.context/id ctx)]]}}))

(defmethod txfn :db/retract [_]
  (fn [ctx multi-color-tx modal-route]
    (let [entity-id (hypercrud.browser.context/id ctx)
          parent-id (-> ctx :hypercrud.browser/parent hypercrud.browser.context/id)
          attr (last (:hypercrud.browser/path ctx))]
      {:tx {(hypercrud.browser.context/uri ctx)
            [[:db/retract parent-id attr entity-id]]}})))
