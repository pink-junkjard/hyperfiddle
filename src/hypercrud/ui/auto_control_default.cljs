(ns hypercrud.ui.auto-control-default
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]))


; Auto-control takes the parent entity as context
; We think this is only used for the dbid, so we can create a tx
; If the parent was someday needed for dispatching, there are better things to dispatch on,
; for example the entire graph can be available in dynamic scope for specific queries, no
; need to limit it to parent.


(defn build-props [entity field anchors param-ctx]
  {:read-only ((get param-ctx :read-only (constantly false)) param-ctx)})


(defn ascertain-valueType [attribute]
  ; todo this should be included in :hyperfiddle/edit-link's :link/renderer
  (if (contains? #{:database/enums
                   :link/renderer
                   :anchor/formula
                   :anchor/visible?
                   :anchor/tx-fn
                   :link/bindings
                   :link-query/value} (-> attribute :attribute/ident))
    :db.type/code
    (-> attribute :attribute/valueType :db/ident)))


(defmethod auto-control/auto-control :default
  [entity field anchors param-ctx]
  (let [{:keys [:attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (ascertain-valueType (:field/attribute field))
        cardinality (:db/ident cardinality)
        props (build-props entity field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/code-editor
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/select-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/table-many-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/table-many-ref
                 (:read-only props) widget/text
                 :else widget/default)]
    (widget entity field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [entity field anchors param-ctx]
  (let [{:keys [:attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        valueType (ascertain-valueType (:field/attribute field))
        cardinality (:db/ident cardinality)
        props (build-props entity field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/input
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one) (not (:read-only props))) widget/instant

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) table-cell/ref-many
                 (and (= cardinality :db.cardinality/many)) table-cell/other-many
                 (:read-only props) widget/text
                 :else widget/default)]
    (widget entity field anchors props param-ctx)))


(defn repeating-links [link param-ctx]
  (->> (:link/anchor link)
       (filter :anchor/repeating?)
       (filter #(nil? (:anchor/field %)))
       (filter #(links/link-visible? % param-ctx))
       (mapv (fn [{:keys [:anchor/link] :as anchor}]
               (let [props (links/build-link-props anchor param-ctx)]
                 ^{:key (:db/id anchor)}
                 [(:navigate-cmp param-ctx) props (:anchor/prompt anchor) param-ctx])))))


(defn non-repeating-links [link param-ctx]
  (->> (:link/anchor link)
       (remove :anchor/repeating?)
       (filter #(nil? (:anchor/field %)))
       (filter #(links/link-visible? % param-ctx))
       (map (fn [{:keys [:anchor/link] :as anchor}]
              (let [props (links/build-link-props anchor param-ctx)]
                ^{:key (:db/id anchor)}
                [(:navigate-cmp param-ctx) props (:anchor/prompt anchor) param-ctx])))))


(defn filter-visible-fields [old-fields param-ctx]
  (filter
    (fn [fieldinfo]
      (let [attr (-> fieldinfo :field/attribute :attribute/ident)
            visible-fn (get-in param-ctx [:fields attr :visible?] (constantly true))]
        (visible-fn param-ctx)))
    old-fields))


(defmethod auto-control/resultset :default [resultset link param-ctx]
  (let [ui-for-resultset (fn [single-result-as-entity? ordered-find-elements resultset link param-ctx]
                           (if single-result-as-entity?
                             (if-let [result (first resultset)]
                               (let [param-ctx (assoc param-ctx :result result)]
                                 [:div
                                  (->> (concat (repeating-links link param-ctx)
                                               (non-repeating-links link (dissoc param-ctx :result)))
                                       (interpose " · "))
                                  [:div
                                   (map (fn [{:keys [:find-element/form] :as find-element}]
                                          (let [entity (get result (:find-element/name find-element))]
                                            ^{:key (hash [(:db/id entity) (:db/id form)])}
                                            [form/form entity form (:link/anchor link) param-ctx]))
                                        ordered-find-elements)]])
                               [:div "No results"])
                             [table/table resultset ordered-find-elements (:link/anchor link) param-ctx]))]
    (condp = (links/link-type link)
      :link-query
      (let [link-query (:link/request link)
            q (some-> link-query :link-query/value reader/read-string)
            single-result-as-entity? (:link-query/single-result-as-entity? link-query)
            ordered-find-elements (->> (find-elements-util/order-find-elements (:link-query/find-element link-query) q)
                                       (mapv (fn [find-element]
                                               (update-in find-element [:find-element/form :form/field] #(filter-visible-fields % param-ctx))))
                                       (remove #(empty? (get-in % [:find-element/form :form/field]))))]
        (ui-for-resultset single-result-as-entity? ordered-find-elements resultset link param-ctx))

      :link-entity
      (let [single-result-as-entity? (map? resultset)
            ordered-find-elements [{:find-element/name :entity
                                    :find-element/form (-> (get-in link [:link/request :link-entity/form])
                                                           (update :form/field #(filter-visible-fields % param-ctx)))}]
            resultset (->> (if single-result-as-entity? [resultset] resultset)
                           (mapv #(assoc {} :entity %)))]
        (ui-for-resultset single-result-as-entity? ordered-find-elements resultset link param-ctx))

      [:div "Unable to render unknown link type"])))
