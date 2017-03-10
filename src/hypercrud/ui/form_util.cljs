(ns hypercrud.ui.form-util
  (:require [hypercrud.util :as util]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.random-color :as random-color]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))

(defn strip-forms-in-raw-mode [ordered-find-elements param-ctx]
  (let [raw-mode? (= (:display-mode param-ctx) :raw)
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

(defn get-ordered-find-elements [link param-ctx]
  (let [req (:link/request link)]
    (case (links/link-type link)
      :link-query (let [q (some-> req :link-query/value reader/read-string)
                        find-element-lookup (find-elements-by-name req)]
                    (->> (util/parse-query-element q :find)
                         (mapv str)
                         (mapv #(get find-element-lookup %))))
      :link-entity [{:find-element/name "entity"
                     :find-element/form (-> (:link-entity/form req)
                                            (update :form/field filter-visible-fields param-ctx))}]
      [])))



(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information through a join. Not all entities are homogenous,
especially consider the '* case, so we need a uniform column set driving the body rows in sync
with the headers but the resultset needs to match this column-fields structure now too; since
the find-element level has been flattened out of the columns."
  [result link param-ctx]
  (let [result (if (map? result) [result] result)           ; unified colspec for table and form
        ordered-find-elements (get-ordered-find-elements link param-ctx)
        ordered-find-elements (strip-forms-in-raw-mode ordered-find-elements param-ctx)
        raw-mode? (= (:display-mode param-ctx) :raw)]
    (vec
      (mapcat (fn [relation-for-fe fe]
                (let [indexed-fields (util/group-by-assume-unique (comp :attribute/ident :field/attribute) (-> fe :find-element/form :form/field))

                      ; find-elements are parsed from the query, so they are known to be good,
                      ; even in raw mode when they haven't been modeled yet.
                      find-element-name (-> fe :find-element/name)

                      entities (map second relation-for-fe)
                      col-idents (if (or raw-mode? (empty? (keys indexed-fields)))
                                   (reduce (fn [acc v] (into acc (keys v))) #{} entities)
                                   (keys indexed-fields))
                      col-idents' (sort-by (fn [k]
                                             (if-let [field (get indexed-fields k)]
                                               (:field/order field)
                                               ; raw mode sort is by namespaced attribute, per find-element
                                               k))
                                           col-idents)]
                  (mapcat (fn [k]
                            [find-element-name k (get indexed-fields k)]) col-idents')))
              (concat (util/transpose result) (repeat {}))  ; Drive from the find elements, the result might be empty
              ordered-find-elements))))

(defn build-props [value maybe-field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})

(defn entity-param-ctx [entity param-ctx]
  (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                   :owner ((:owner-fn param-ctx) entity param-ctx)
                   :entity entity))

(def connection-color
  (memoize
    (fn [conn-id]
      (case conn-id
        nil "#fff"
        hc/*root-conn-id* "#777"
        (random-color/random-color)))))
