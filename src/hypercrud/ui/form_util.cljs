(ns hypercrud.ui.form-util
  (:require [hypercrud.util :as util]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]))


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
      (mapcat (fn [resultset-for-fe fe]                     ; misnamed
                (let [indexed-fields (util/group-by-assume-unique (comp :attribute/ident :field/attribute) (-> fe :find-element/form :form/field))
                      find-element-name (ffirst resultset-for-fe)

                      entities (map second resultset-for-fe)
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
              (util/transpose result)
              ordered-find-elements))))