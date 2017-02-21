(ns hypercrud.ui.auto-control-default
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.types :as types]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util :as util]
            [hypercrud.ui.input :as input]
            [hypercrud.client.tx :as tx]
            [hypercrud.browser.system-links :as system-links]))


(defn build-props [value field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  {:read-only ((get param-ctx :read-only) field param-ctx)})


(defmethod auto-control/auto-control :default
  [value field anchors param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        props (build-props value field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/code-editor
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) widget/select-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many) isComponent) widget/table-many-ref-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) widget/table-many-ref
                 :else (constantly [:div (pr-str [value valueType cardinality isComponent])]) ;widget/default
                 )]
    (widget value field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [value field anchors param-ctx]
  (let [isComponent (-> (:attribute param-ctx) :attribute/isComponent)
        valueType (-> (:attribute param-ctx) :attribute/valueType :db/ident)
        cardinality (-> (:attribute param-ctx) :attribute/cardinality :db/ident)
        props (build-props value field anchors param-ctx)
        widget (cond
                 (and (= valueType :db.type/boolean) (= cardinality :db.cardinality/one)) widget/select-boolean
                 (and (= valueType :db.type/keyword) (= cardinality :db.cardinality/one)) widget/input-keyword
                 (and (= valueType :db.type/string) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/long) (= cardinality :db.cardinality/one)) widget/input-long
                 (and (= valueType :db.type/code) (= cardinality :db.cardinality/one)) widget/input
                 (and (= valueType :db.type/instant) (= cardinality :db.cardinality/one)) widget/instant

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one) isComponent) table-cell/ref-one-component
                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/one)) widget/select-ref

                 (and (= valueType :db.type/ref) (= cardinality :db.cardinality/many)) table-cell/ref-many
                 (and (= cardinality :db.cardinality/many)) table-cell/other-many
                 (:read-only props) widget/text
                 :else (constantly [:div (pr-str [value valueType cardinality isComponent])]) ;widget/default
                 )]
    (widget value field anchors props param-ctx)))


(defn filter-visible-fields [old-fields param-ctx]
  (filter
    (fn [fieldinfo]
      (let [attr (-> fieldinfo :field/attribute :attribute/ident)
            visible-fn (get-in param-ctx [:fields attr :visible?] (constantly true))]
        (visible-fn param-ctx)))
    old-fields))


(defn no-link-type [anchors param-ctx]
  (let [non-repeating-top-anchors (->> anchors
                                       (remove :anchor/repeating?)
                                       (remove :anchor/find-element)
                                       (remove :anchor/field))]
    [:div
     "Unable to render unknown link type"
     [:br]
     (widget/render-anchors (remove :anchor/render-inline? non-repeating-top-anchors) param-ctx)
     (let [param-ctx (dissoc param-ctx :isComponent)]
       (widget/render-inline-links (filter :anchor/render-inline? non-repeating-top-anchors) param-ctx))]))


(defmethod auto-control/resultset :default [resultset link param-ctx]
  (let [ui-for-resultset (fn [single-result-as-entity?]
                           (if single-result-as-entity?
                             (if (system-links/system-link? (:db/id link)) #_(= :system-edit (-> link :db/id :id :link/ident))
                               form/sys-form
                               form/form)
                             table/table))]
    (case (links/link-type link)
      :link-query
      (let [link-query (:link/request link)
            q (some-> link-query :link-query/value reader/read-string)
            ui (ui-for-resultset (:link-query/single-result-as-entity? link-query))
            find-element-lookup (->> (mapv (juxt :find-element/name identity) (:link-query/find-element link-query))
                                     (into {}))
            ordered-find-elements
            (->> (util/parse-query-element q :find)
                 (mapv str)
                 (mapv #(get find-element-lookup %))
                 (mapv (fn [find-element]
                         (update-in find-element [:find-element/form :form/field] #(filter-visible-fields % param-ctx)))))]
        [ui resultset ordered-find-elements (:link/anchor link) param-ctx])

      :link-entity
      (let [single-result-as-entity? (map? resultset)       ;todo this is broken when no results are returned
            ui (ui-for-resultset single-result-as-entity?)
            ordered-find-elements [{:find-element/name "entity"
                                    :find-element/form (-> (get-in link [:link/request :link-entity/form])
                                                           (update :form/field #(filter-visible-fields % param-ctx)))}]
            resultset (->> (if single-result-as-entity? [resultset] resultset)
                           (mapv #(assoc {} "entity" %)))]
        [ui resultset ordered-find-elements (:link/anchor link) param-ctx])

      [no-link-type (:link/anchor link) param-ctx])))


(defmethod auto-control/raw-control :default
  [value anchors param-ctx]
  (let [props {}]
    [widget/raw value anchors props param-ctx]))

(defmethod auto-control/raw-table-cell :default
  [value anchors param-ctx]
  (let [props {}]
    [widget/raw value anchors props param-ctx]))