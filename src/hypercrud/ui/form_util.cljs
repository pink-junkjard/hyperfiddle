(ns hypercrud.ui.form-util
  (:require [hypercrud.util :as util]))


(defn strip-forms-in-raw-mode [ordered-find-elements param-ctx]
  (let [raw-mode? (= (:display-mode param-ctx) :raw)
        f (if raw-mode? #(dissoc % :find-element/form) identity)]
    (map f ordered-find-elements)))



(defn determine-colspec "Colspec is what you have when you flatten out the find elements,
but retaining and correlating all information through a join"
  [resultset ordered-find-elements param-ctx]

  (let [raw-mode? (= (:display-mode param-ctx) :raw)
        ordered-find-elements (strip-forms-in-raw-mode ordered-find-elements param-ctx)]
    (vec
      (mapcat (fn [resultset-for-fe fe]
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
              (util/transpose resultset)
              ordered-find-elements))))