(ns hypercrud.browser.auto-form
  (:require [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.util.core :as util]))


(defn system-field? [field-dbid]
  (map? (:id field-dbid)))

(defn auto-find-elements [ordered-fes result param-ctx]
  (let [raw-mode? (= @(:display-mode param-ctx) :root)
        result (if (map? result) [result] result)
        results-indexed-by-column (->> (apply concat result)
                                       (group-by first)
                                       (util/map-values #(map second %)))]
    ; find-elements are parsed from the query, so they are known to be good,
    ; even in raw mode when they haven't been modeled yet.
    (->> ordered-fes
         (map (fn [fe]
                (let [splat? (or raw-mode? (empty? (-> fe :find-element/form :form/field)))
                      fields (if-not splat?
                               (-> fe :find-element/form :form/field)
                               (->> (get results-indexed-by-column (:find-element/name fe))
                                    (reduce (fn [acc v] (into acc (keys (dissoc v :db/id)))) #{})
                                    (map (fn [ident] {:db/id (->DbId {:fe (-> fe :db/id :id) :a ident} (:code-database-uri param-ctx))
                                                      :field/attribute ident}))))
                      ; raw mode sort is by namespaced attribute, per find-element
                      sort-fn (if splat? :field/attribute :field/order)]
                  (->> fields
                       (sort-by sort-fn)
                       (assoc-in fe [:find-element/form :form/field])))))
         (vec))))
