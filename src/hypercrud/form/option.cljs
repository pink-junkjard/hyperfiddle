(ns hypercrud.form.option
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.form.find-elements :as find-elements-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbVal]]))


(defn label-prop [{{find-elements :link/find-element} :field/options-link :as field} result]
  (->> (mapcat (fn [{{fields :form/field} :find-element/form} entity]
                 (->> fields
                      (mapv #(-> % :field/attribute :attribute/ident))
                      (mapv #(get entity %))))
               find-elements result)
       (interpose ", ")
       (apply str)))


(defn get-key [{{find-elements :link/find-element :as link} :field/options-link :as field}]
  (hash [(:link/query link) (->> find-elements
                                 (mapcat #(-> % :find-element/form :form/field))
                                 (mapv #(-> % :field/attribute :attribute/ident)))]))


;todo should be get-queries and we can delete hc.form.util/field-queries
(defn get-query [{{find-elements :link/find-element :as link} :field/options-link :as field} p-filler param-ctx]
  (if-let [q (let [q (:link/query link)]
               (if-not (empty? q)
                 (reader/read-string q)))]
    (let [params (p-filler link param-ctx)
          pull-exp (->> (mapv (juxt :find-element/name
                                    (fn [{:keys [:find-element/connection :find-element/form] :as find-element}]
                                      [(->DbVal (-> connection :db/id :id) nil)
                                       (conj (mapv #(-> % :field/attribute :attribute/ident)
                                                   (:form/field form))
                                             :db/id)]))
                              find-elements)
                        (into {}))
          query-value [q params pull-exp]]
      {(hash query-value) query-value})))


(defn get-option-records [{{find-elements :link/find-element q :link/query :as link} :field/options-link :as field} super-graph query-params]
  (if-let [q (if-not (empty? q)
               (reader/read-string q))]
    (let [params (merge query-params (q-util/build-dbhole-lookup link))
          p-filler (fn [link param-ctx]
                     (q-util/build-params #(get params %) link param-ctx))
          ordered-find-elements (find-elements-util/order-find-elements find-elements q)]
      (->> (hc/select super-graph (-> (get-query field p-filler nil)
                                      keys
                                      first))
           (mapv (fn [result]
                   (mapv (fn [find-element]
                           (let [dbid (get result (:find-element/name find-element))
                                 dbval (->DbVal (-> find-element :find-element/connection :db/id :id) nil)
                                 dbgraph (hc/get-dbgraph super-graph dbval)]
                             (hc/entity dbgraph dbid)))
                         ordered-find-elements)))))))
