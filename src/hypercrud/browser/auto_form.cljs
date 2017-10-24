(ns hypercrud.browser.auto-form
  (:require [hypercrud.util.core :as util]))


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
                (let [splat? (or raw-mode? (empty? (get-in fe [:find-element/form :form/field])))]
                  (update fe :find-element/form
                          (fn [form]
                            (-> (into {} form)
                                (update :form/field
                                        (fn [fields]
                                          (if-not splat?
                                            (sort-by :field/order fields)
                                            (->> (get results-indexed-by-column (:find-element/name fe))
                                                 (reduce (fn [acc v]
                                                           (-> (keys v)
                                                               (set)
                                                               (disj :db/id)
                                                               (into acc)))
                                                         #{})
                                                 (map (fn [ident] {:db/id {:fe (get-in fe [:db/id :id]) :a ident}
                                                                   :field/attribute ident}))
                                                 ; raw mode sort is by namespaced attribute, per find-element
                                                 (sort-by :field/attribute)))))))))))
         (vec))))
