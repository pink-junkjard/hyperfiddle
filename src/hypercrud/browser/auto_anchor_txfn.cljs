(ns hypercrud.browser.auto-anchor-txfn
  (:require [hypercrud.types :refer [->DbId]]
            [hypercrud.client.tx :as tx]
            [promesa.core :as p]))


(defn auto-txfn [anchor]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute ident :anchor/ident} anchor]
    (cond

      ; link-query's always have a find-element
      ; link-entity's never do
      ; that bit is not interesting here.

      (and (not r) a)                                       ; attr create
      (pr-str `(fn [ctx# show-popover!#]
                 (let [new-dbid# (->DbId (-> (str (-> ctx# :entity :db/id :id) "."
                                                  (-> ctx# :attribute :attribute/ident) "."
                                                  (case (-> ((:schema ctx#) (-> ctx# :attribute :attribute/ident)) :attribute/cardinality :db/ident)
                                                    :db.cardinality/one nil
                                                    :db.cardinality/many (inc (count (:value ctx#)))))
                                             hash js/Math.abs - str)
                                         (-> ctx# :entity :db/id :conn-id))]
                   (-> (show-popover!#)
                       (p/then (fn [tx-from-modal#]
                                 {:tx (concat
                                        tx-from-modal#
                                        (tx/edit-entity (-> ctx# :entity :db/id)
                                                        (-> ctx# :attribute :attribute/ident)
                                                        [] [new-dbid#]))}))))))

      (and r (not a) (= ident :remove))
      (pr-str `(fn [ctx#]
                 {:tx [[:db.fn/retractEntity (-> ctx# :entity :db/id)]]}))


      :else nil)))
