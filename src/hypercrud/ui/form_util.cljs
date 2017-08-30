(ns hypercrud.ui.form-util
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]))

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

(defn get-ordered-find-elements [link query-params param-ctx]
  (let [fill-fe-conn-id (fn [fe]
                          (let [conn-dbid (->DbId (q-util/fe-conn-id query-params fe)
                                                  ; this conn-id doesn't matter and will be removed anyway
                                                  hc/*root-conn-id*)]
                            (assoc-in fe [:find-element/connection :db/id] conn-dbid)))]
    (case (:request/type link)
      ; this could throw, we only run this code after a link has returned successfully, getting lucky here
      :query (let [q (some-> link :link-query/value reader/read-string)
                   find-element-lookup (->> (:link-query/find-element link)
                                            (map (juxt :find-element/name fill-fe-conn-id))
                                            (into {}))]
               (->> (util/parse-query-element q :find)
                    (mapv str)
                    (mapv #(get find-element-lookup %))))
      :entity [(-> (->> (:link-query/find-element link)
                        (filter #(= (:find-element/name %) "entity"))
                        first)
                   (fill-fe-conn-id))]
      [])))

(defn fe->db [fe param-ctx]
  (let [fe-conn (:find-element/connection fe)]
    (let [conn-id (-> fe-conn :db/id :id)
          branch (:branch param-ctx)]
      (hc/db (:peer param-ctx) conn-id branch))))

(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information through a join. Not all entities are homogenous,
especially consider the '* case, so we need a uniform column set driving the body rows in sync
with the headers but the resultset needs to match this column-fields structure now too; since
the find-element level has been flattened out of the columns."
  ; Need result only for raw mode.
  [result link schemas query-params param-ctx]
  (let [result (if (map? result) [result] result)           ; unified colspec for table and form
        ordered-find-elements (-> (get-ordered-find-elements link query-params param-ctx)
                                  (strip-forms-in-raw-mode param-ctx))
        raw-mode? (= (:display-mode param-ctx) :root)
        result-as-columns (util/transpose result)
        map' (util/map-pad {})]
    (->>
      (map' (fn [relation-for-fe fe]
              (let [indexed-fields (util/group-by-assume-unique :field/attribute (-> fe :find-element/form :form/field))

                    ; find-elements are parsed from the query, so they are known to be good,
                    ; even in raw mode when they haven't been modeled yet.

                    col-idents (if (or raw-mode? (empty? (keys indexed-fields)))
                                 (let [entities (map second relation-for-fe)]
                                   (reduce (fn [acc v] (into acc (keys (dissoc v :db/id)))) #{} entities))
                                 (keys indexed-fields))
                    col-idents' (sort-by (fn [k]
                                           (if-let [field (get indexed-fields k)]
                                             (:field/order field)
                                             ; raw mode sort is by namespaced attribute, per find-element
                                             k))
                                         col-idents)
                    db (fe->db fe param-ctx)
                    schema (get schemas (:find-element/name fe))]
                (mapcat (fn [ident]
                          (let [attr (get schema ident {:db/ident ident})
                                field (get indexed-fields ident)]
                            [db fe attr field])) col-idents')))
            result-as-columns
            ordered-find-elements)
      (flatten)
      (vec))))

(defn build-props [maybe-field anchors param-ctx]
  ; why does this need the field - it needs the ident for readonly in "Edit Anchors"
  ; todo clean this interface up
  {:read-only ((get param-ctx :read-only) (:attribute param-ctx) param-ctx)})

(defn attribute-human [attr]
  (-> attr
      (dissoc :db/id)
      (util/update-existing :db/cardinality :db/ident)
      (util/update-existing :db/valueType :db/ident)
      (util/update-existing :db/unique :db/ident)
      (util/update-existing :attribute/hc-type :hc-type/name)))

(defn field-label [maybe-field param-ctx]
  (let [docstring (-> maybe-field :field/doc)
        field-prompt (util/fallback empty? (get maybe-field :field/prompt) (-> param-ctx :attribute :db/ident str))]
    [tooltip/hover-popover-managed
     {:label (case (:display-mode param-ctx)
               ; (auto-control maybe-field anchors props param-ctx)
               :user (if-not (empty? docstring) (markdown docstring #()))
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