(ns hypercrud.ui.form-util
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbId]]
            [hypercrud.ui.code-editor :as code-editor]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util :as util]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))

(defn strip-forms-in-raw-mode [ordered-find-elements param-ctx]
  (let [raw-mode? (= (:display-mode param-ctx) :root)
        f (if raw-mode? #(dissoc % :find-element/form) identity)]
    (map f ordered-find-elements)))

(defn filter-visible-fields [fields param-ctx]
  (filter
    (fn [fieldinfo]
      (let [attr (-> fieldinfo :field/attribute :attribute/ident)
            visible-fn (get-in param-ctx [:fields attr :visible?] (constantly true))]
        (visible-fn param-ctx)))
    fields))

(defn find-elements-by-name [link-req]
  (->> (mapv (juxt :find-element/name identity) (:link-query/find-element link-req))
       (into {})))

(defn manufacture-entity-find-element [link #_branches param-ctx]
  (let [conn (-> link :link/request :link-entity/connection)
        #_(or
               #_(-> link :link/request :link-entity/connection)
               (let [dbid-s (-> param-ctx :query-params :entity)]
                 {:db/id (->DbId (if (vector? dbid-s)
                                   (:conn-id (first dbid-s))
                                   (:conn-id dbid-s))
                                 nil #_"ignored in the place we need it, ->entityRequest")}))]
    (assert conn)
    {:db/id "entity"                                        ; sentinel
     :find-element/name "entity"
     :find-element/connection conn
     :find-element/form (-> (-> link :link/request :link-entity/form)
                            (update :form/field filter-visible-fields param-ctx))}))

(defn get-ordered-find-elements [link #_branches param-ctx]
  (let [req (:link/request link)]
    (case (link-util/link-type link)
      :link-query (let [q (some-> req :link-query/value reader/read-string)
                        find-element-lookup (find-elements-by-name req)]
                    (->> (util/parse-query-element q :find)
                         (mapv str)
                         (mapv #(get find-element-lookup %))))
      :link-entity [(manufacture-entity-find-element link #_branches param-ctx)]
      [])))



(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information through a join. Not all entities are homogenous,
especially consider the '* case, so we need a uniform column set driving the body rows in sync
with the headers but the resultset needs to match this column-fields structure now too; since
the find-element level has been flattened out of the columns."
  [result link branch-or-branches param-ctx]
  (let [result (if (map? result) [result] result)           ; unified colspec for table and form
        branches (if-not (map? branch-or-branches) {"entity" branch-or-branches} branch-or-branches)
        ordered-find-elements (get-ordered-find-elements link #_branches param-ctx)
        ordered-find-elements (strip-forms-in-raw-mode ordered-find-elements param-ctx)
        raw-mode? (= (:display-mode param-ctx) :root)
        result-as-columns (util/transpose result)
        map' (util/map-pad {})]
    (->>
      (map' (fn [relation-for-fe fe]
              (let [indexed-fields (util/group-by-assume-unique (comp :attribute/ident :field/attribute) (-> fe :find-element/form :form/field))

                    ; find-elements are parsed from the query, so they are known to be good,
                    ; even in raw mode when they haven't been modeled yet.
                    fe-name (-> fe :find-element/name)
                    fe-conn (-> fe :find-element/connection)

                    entities (map second relation-for-fe)
                    col-idents (if (or raw-mode? (empty? (keys indexed-fields)))
                                 (reduce (fn [acc v] (into acc (keys v))) #{} entities)
                                 (keys indexed-fields))
                    col-idents' (sort-by (fn [k]
                                           (if-let [field (get indexed-fields k)]
                                             (:field/order field)
                                             ; raw mode sort is by namespaced attribute, per find-element
                                             k))
                                         col-idents)

                    db (hc/db (:response param-ctx) (-> fe-conn :db/id :id) (get branches fe-name))]
                (mapcat (fn [k]
                          [db fe-name k (get indexed-fields k)]) col-idents')))
            result-as-columns
            ordered-find-elements)
      (flatten)
      (vec))))

(defn link-entity-colspec [result link param-ctx]
  (let [result (if (map? result) [result] result)]

    ))

(defn build-props [maybe-field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})

(defn entity-param-ctx [entity param-ctx]
  (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                   :owner ((:owner-fn param-ctx) entity param-ctx)
                   :entity entity))

(defn attribute-human [attr]
  (-> attr
      (dissoc :db/id)
      (util/update-existing :attribute/cardinality :db/ident)
      (util/update-existing :attribute/valueType :db/ident)
      (util/update-existing :attribute/unique :db/ident)
      (util/update-existing :attribute/hc-type :hc-type/name)))

(defn field-label [maybe-field param-ctx]
  (let [docstring (-> maybe-field :field/doc)
        field-prompt (util/fallback empty? (get maybe-field :field/prompt) (-> param-ctx :attribute :attribute/ident str))]
    [tooltip/hover-popover-managed
     {:label (case (:display-mode param-ctx)
               :user (if-not (empty? docstring) [:pre docstring])
               :xray [:pre (util/pprint-str (attribute-human (:attribute param-ctx)) 50)])}
     [:span {:class (case (:display-mode param-ctx)
                      :user (if-not (empty? docstring) "help")
                      :xray "help")} field-prompt]]
    #_[:div
       (let [is-ref? (coll? value)]
         (if is-ref?
           [tooltip/click-popover-managed
            {:body [code-editor/code-editor* (util/pprint-str value 100) nil {:readOnly true}]}
            [:a {:href "javascript:void 0;"} "§"]]))
       " "
       ]))