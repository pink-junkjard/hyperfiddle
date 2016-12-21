(ns hypercrud.ui.auto-control-default
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.select :as select]
            [hypercrud.ui.widget :as widget]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


; Auto-control takes the parent entity as context
; We think this is only used for the dbid, so we can create a tx
; If the parent was someday needed for dispatching, there are better things to dispatch on,
; for example the entire graph can be available in dynamic scope for specific queries, no
; need to limit it to parent.


(defn build-props [entity field link-ctxs param-ctx]
  {:read-only (if-let [read-only (:read-only param-ctx)] (read-only param-ctx))})


(defmethod auto-control/auto-control :default
  [entity field link-ctxs param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (:db/ident valueType)
        cardinality (:db/ident cardinality)
        props (build-props entity field link-ctxs param-ctx)
        widget (cond
                 (if-let [read-only (:read-only param-ctx)] (read-only param-ctx)) widget/text
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) select/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/code-editor
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/select-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/table-many-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/table-many-ref
                 :else widget/default)]
    (widget entity field link-ctxs props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [entity field link-ctxs param-ctx]
  (let [{:keys [:attribute/valueType :attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (:db/ident valueType)
        cardinality (:db/ident cardinality)
        props (build-props entity field link-ctxs param-ctx)
        widget (cond
                 (if-let [read-only (:read-only param-ctx)] (read-only param-ctx)) widget/text
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) select/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) table-cell/ref-many
                 (and (= cardinality :db.cardinality/many)) table-cell/other-many

                 :else widget/default)]
    (widget entity field link-ctxs props param-ctx)))


(defn repeating-links [link result param-ctx]
  (->> (:link/link-ctx link)
       (filter :link-ctx/repeating?)
       (filter #(nil? (:link-ctx/field %)))
       (mapv (fn [{:keys [:link-ctx/link] :as link-ctx}]
               (let [param-ctx (merge param-ctx {:result result})
                     props (links/query-link link-ctx param-ctx)]
                 ^{:key (:db/id link)}
                 [(:navigate-cmp param-ctx) props (:link/prompt link) param-ctx])))))


(defn non-repeating-links [link param-ctx]
  (->> (:link/link-ctx link)
       (remove :link-ctx/repeating?)
       (filter #(nil? (:link-ctx/field %)))
       (map (fn [{:keys [:link-ctx/link] :as link-ctx}]
              (let [props (links/query-link link-ctx param-ctx)]
                ^{:key (:db/id link)}
                [(:navigate-cmp param-ctx) props (:link/prompt link) param-ctx])))))


(defmethod auto-control/resultset :default [resultset link param-ctx]
  (let [q (some-> link :link/query reader/read-string)
        ordered-find-elements (find-elements-util/order-find-elements (:link/find-element link) q)]
    (if (:link/single-result-as-entity? link)
      (let [result (first resultset)]
        [:div
         (->> (concat (repeating-links link result param-ctx)
                      (non-repeating-links link param-ctx))
              (interpose " · "))
         (let [param-ctx (assoc param-ctx :result result)]
           [:div
            (map (fn [{:keys [:find-element/form] :as find-element}]
                   (let [entity (get result (:find-element/name find-element))]
                     ^{:key (hash [(.-dbid entity) (.-dbid form)])}
                     [form/form entity form (:link/link-ctx link) param-ctx]))
                 ordered-find-elements)])])
      ^{:key (hc/t (:super-graph param-ctx))}
      [table/table resultset ordered-find-elements (:link/link-ctx link) param-ctx])))