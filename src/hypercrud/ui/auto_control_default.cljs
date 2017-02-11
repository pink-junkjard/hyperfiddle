(ns hypercrud.ui.auto-control-default
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.types :as types]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.table-cell :as table-cell]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util :as util]))


; Auto-control takes the parent entity as context
; We think this is only used for the dbid, so we can create a tx
; If the parent was someday needed for dispatching, there are better things to dispatch on,
; for example the entire graph can be available in dynamic scope for specific queries, no
; need to limit it to parent.


(defn build-props [value field anchors param-ctx]
  {:read-only ((get param-ctx :read-only (constantly false)) field param-ctx)})


(defn determine-value-type [value field]
  (cond
    (keyword? value) :db.type/keyword
    (string? value) :db.type/string
    (or (= true value) (= false value)) :db.type/boolean
    (number? value) :db.type/long
    ;:db.type/bigint :db.type/float :db.type/double :db.type/bigdec
    (map? value) :db.type/ref
    (instance? js/Date value) :db.type/instant
    (instance? types/DbId value) :db.type/ref
    ;:db.type/uuid
    ;:db.type/uri
    ;:db.type/bytes
    :else (-> field :field/attribute :attribute/valueType :db/ident)))


(defmethod auto-control/auto-control :default
  [value field anchors param-ctx]
  (let [
        ;{:keys [:attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        ;valueType (ascertain-valueType (:field/attribute field))
        ;cardinality (:db/ident cardinality)
        cardinality (if (vector? value) :db.cardinality/many :db.cardinality/one)

        valueType (if (= cardinality :db.cardinality/many)
                    (determine-value-type (first value) field)
                    (determine-value-type value field))

        props (build-props value field anchors param-ctx)
        isComponent false

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
                 :else (constantly [:div (pr-str [value valueType cardinality isComponent])]) ;widget/default
                 )]
    (widget value field anchors props param-ctx)))


(defmethod auto-control/auto-table-cell :default
  [value field anchors param-ctx]
  (let [
        ;{:keys [:attribute/cardinality :attribute/isComponent]} (:field/attribute field)
        ;valueType (ascertain-valueType (:field/attribute field))
        ;cardinality (:db/ident cardinality)
        cardinality (if (vector? value) :db.cardinality/many :db.cardinality/one)

        valueType (if (= cardinality :db.cardinality/many)
                    (determine-value-type (first value) field)
                    (determine-value-type value field))

        props (build-props value field anchors param-ctx)
        isComponent false

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


(defn fields-for-entity [entity]
  (->> (keys entity)
       (mapv (fn [attr]
               {:field/attribute {:attribute/ident attr}
                :field/prompt attr}))))


(defn fields-for-entity-list [entity-list]
  (->> (mapcat keys entity-list)
       (set)
       (mapv (fn [attr]
               {:field/attribute {:attribute/ident attr}
                :field/prompt attr}))))


(defmethod auto-control/resultset :default [resultset link param-ctx]
  (let [ui-for-resultset (fn [single-result-as-entity?] (if single-result-as-entity? form/forms-list table/table))]
    (case (links/link-type link)
      :link-query
      (let [link-query (:link/request link)
            q (some-> link-query :link-query/value reader/read-string)
            ui (ui-for-resultset (:link-query/single-result-as-entity? link-query))
            find-element-lookup (->> (mapv (juxt :find-element/name identity) (:link-query/find-element link-query))
                                     (into {}))
            ordered-find-elements (->> (util/parse-query-element q :find)
                                       (mapv str)
                                       (mapv (fn [find-element-name]
                                               (-> (get find-element-lookup find-element-name)
                                                   (update :find-element/form (fn [old-form]
                                                                                (or old-form
                                                                                    {:form/field (if (:link-query/single-result-as-entity? link-query)
                                                                                                   (fields-for-entity (get (first resultset) find-element-name))
                                                                                                   (fields-for-entity-list (mapv #(get % find-element-name) resultset)))}))))))
                                       (mapv (fn [find-element]
                                               (update-in find-element [:find-element/form :form/field] #(filter-visible-fields % param-ctx)))))]
        [ui resultset ordered-find-elements (:link/anchor link) param-ctx])

      :link-entity
      (let [single-result-as-entity? (map? resultset)       ;todo this is broken when no results are returned
            ui (ui-for-resultset single-result-as-entity?)
            ordered-find-elements [{:find-element/name :entity
                                    :find-element/form (-> (get-in link [:link/request :link-entity/form]
                                                                   {:form/field (if single-result-as-entity?
                                                                                  (fields-for-entity resultset)
                                                                                  (fields-for-entity-list resultset))})
                                                           (update :form/field #(filter-visible-fields % param-ctx)))}]
            resultset (->> (if single-result-as-entity? [resultset] resultset)
                           (mapv #(assoc {} :entity %)))]
        [ui resultset ordered-find-elements (:link/anchor link) param-ctx])

      [no-link-type (:link/anchor link) param-ctx])))
