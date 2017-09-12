(ns hypercrud.ui.form-util
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.ui.markdown :refer [markdown]]
            [hypercrud.ui.tooltip :as tooltip]
            [hypercrud.util.core :as util]
            [hypercrud.util.monad :refer [exception->either]]))

(defn css-slugify [s]
  ; http://stackoverflow.com/a/449000/959627
  (-> s
      (string/replace ":" "-")
      (string/replace "/" "-")
      (string/replace " " "-")))

(defn strip-form-in-raw-mode [fe param-ctx]
  (if (= (:display-mode param-ctx) :root)
    (dissoc fe :find-element/form)
    fe))

(defn get-ordered-find-elements [link query-params param-ctx]
  (mlet [fes (case (:request/type link)
               :query (let [find-element-lookup (->> (:link-query/find-element link)
                                                     (map (juxt :find-element/name identity))
                                                     (into {}))]
                        (mlet [q (q-util/safe-parse-query-validated link)]
                          (->> (util/parse-query-element q :find)
                               (mapv str)
                               (mapv #(get find-element-lookup % {:find-element/name %}))
                               (cats/return))))
               :entity (either/right [(or (->> (:link-query/find-element link)
                                               (filter #(= (:find-element/name %) "entity"))
                                               first)
                                          {:find-element/name "entity"})])
               (either/right []))
         :let [fill-fe-conn-id (fn [fe]
                                 (let [conn-dbid (->DbId (q-util/fe-conn-id query-params fe)
                                                         ; this conn-id doesn't matter and will be removed anyway
                                                         hc/*root-conn-id*)]
                                   (assoc-in fe [:find-element/connection :db/id] conn-dbid)))]]
    (->> fes
         (map fill-fe-conn-id)
         (map #(strip-form-in-raw-mode % param-ctx))
         (cats/return))))

(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information through a join. Not all entities are homogenous,
especially consider the '* case, so we need a uniform column set driving the body rows in sync
with the headers but the resultset needs to match this column-fields structure now too; since
the find-element level has been flattened out of the columns."
  ; Need result only for raw mode.
  [result schemas ordered-fes param-ctx]
  (let [result (if (map? result) [result] result)           ; unified colspec for table and form
        raw-mode? (= (:display-mode param-ctx) :root)
        results-indexed-by-column (->> (apply concat result)
                                       (group-by first)
                                       (util/map-values #(map second %)))]

    ; find-elements are parsed from the query, so they are known to be good,
    ; even in raw mode when they haven't been modeled yet.
    (->> ordered-fes
         (map (fn [fe]
                (let [indexed-fields (util/group-by-assume-unique :field/attribute (-> fe :find-element/form :form/field))
                      col-idents (if (or raw-mode? (empty? (keys indexed-fields)))
                                   (let [entities (get results-indexed-by-column (:find-element/name fe))]
                                     (reduce (fn [acc v] (into acc (keys (dissoc v :db/id)))) #{} entities))
                                   (keys indexed-fields))
                      sort-fn (fn [k]
                                (if-let [field (get indexed-fields k)]
                                  (:field/order field)
                                  ; raw mode sort is by namespaced attribute, per find-element
                                  k))
                      schema (get schemas (:find-element/name fe))]
                  {:fe fe
                   :fe-colspec (->> col-idents
                                    (sort-by sort-fn)
                                    (map (fn [ident]
                                           {:attr (get schema ident {:db/ident ident})
                                            :maybe-field (get indexed-fields ident)})))})))

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