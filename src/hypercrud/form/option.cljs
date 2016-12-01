(ns hypercrud.form.option
  (:require [cljs.reader :as reader]
            [hypercrud.client.core :as hc]
            [hypercrud.types :refer [->DbVal]]))


(defn label-prop [{{find-elements :link/find-element} :field/options-link :as field} result]
  (->> (mapcat (fn [{{fields :form/field} :find-element/form} entity]
                 (->> fields
                      (mapv #(-> % :field/attribute :attribute/ident))
                      (mapv #(get entity %))))
               find-elements result)
       (interpose ", ")
       (apply str)))


(defn get-key [{{find-elements :link/find-element query :link/query} :field/options-link :as field}]
  (hash [(:query/value query) (->> find-elements
                                   (mapcat #(-> % :find-element/form :form/field))
                                   (mapv #(-> % :field/attribute :attribute/ident)))]))


(defn get-option-records [{{find-elements :link/find-element query :link/query} :field/options-link :as field} graph entity]
  (if-let [q (let [q (:query/value query)]
               (if-not (empty? q)
                 (reader/read-string q)))]
    (let [dbgraph (.-dbgraph entity)]
      (->> (hc/select graph (hash q) q)
           (mapv (fn [result]
                   ;todo zip result with find-elements and use :find-element/connection
                   (mapv #(hc/entity dbgraph %) result)))))))


;todo should be get-queries and we can delete hc.form.util/field-queries
(defn get-query [{{formulas :link/formula query :link/query find-elements :link/find-element} :field/options-link :as field} p-filler param-ctx]
  (if-let [q (let [q (:query/value query)]
               (if-not (empty? q)
                 (reader/read-string q)))]
    (let [query-name (hash q)
          params (p-filler query formulas param-ctx)
          pull-dbval (get param-ctx :dbval)
          pull-exp (->> (mapv (juxt :find-element/name
                                    (fn [{:keys [:find-element/form] :as find-element}]
                                      ; todo use :find-element/connection
                                      [pull-dbval (conj (mapv #(-> % :field/attribute :attribute/ident)
                                                              (:form/field form))
                                                        :db/id)]))
                              find-elements)
                        (into {}))]
      {query-name [q params pull-exp]})))
