(ns hypercrud.form.option
  (:require [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbVal]]
            [hypercrud.ui.form :as form]))


(defn label-prop [field result]
  (let [find-elements (-> field :field/options-link-ctx :link-ctx/link :link/find-element)]
    (->> (mapcat (fn [{{fields :form/field} :find-element/form} entity]
                   (->> fields
                        (mapv #(-> % :field/attribute :attribute/ident))
                        (mapv #(get entity %))))
                 find-elements result)
         (interpose ", ")
         (apply str))))


(defn get-key [field]
  (let [link (-> field :field/options-link-ctx :link-ctx/link)]
    (hash [(:link/query link) (->> (:link/find-element link)
                                   (mapcat #(-> % :find-element/form :form/field))
                                   (mapv #(-> % :field/attribute :attribute/ident)))])))


(defn get-option-records [field param-ctx]
  (let [{find-elements :link/find-element q :link/query :as link} (-> field :field/options-link-ctx :link-ctx/link)]
    (if-let [q (if-not (empty? q)
                 (reader/read-string q))]
      (let [ordered-find-elements (find-elements-util/order-find-elements find-elements q)
            result-query (let [params-map (merge (:query-params (links/build-params-map (:field/options-link-ctx field) param-ctx))
                                                 (q-util/build-dbhole-lookup link))
                               params (q-util/build-params #(get params-map %) link param-ctx)
                               pull-exp (form/query-pull-exp find-elements)]
                           [q params pull-exp])]
        (->> (hc/select (:super-graph param-ctx) (hash result-query))
             (mapv (fn [result]
                     (mapv (fn [find-element]
                             (let [dbid (get result (:find-element/name find-element))
                                   dbval (->DbVal (-> find-element :find-element/connection :db/id :id) nil)
                                   dbgraph (hc/get-dbgraph (:super-graph param-ctx) dbval)]
                               (hc/entity dbgraph dbid)))
                           ordered-find-elements))))))))
