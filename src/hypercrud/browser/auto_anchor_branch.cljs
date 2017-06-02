(ns hypercrud.browser.auto-anchor-branch
  (:require [hypercrud.types :refer [->DbId]]
            [hypercrud.client.tx :as tx]
            [promesa.core :as p]))


(defn auto-branch [anchor]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute} anchor]
    (cond

      ; only care about managed cases

      ; attr create
      (and (not r) a)
      (pr-str `(fn [ctx#]
                 (-> (str (-> ctx# :entity :db/id :id) "."
                          (-> ctx# :attribute :attribute/ident) "."
                          (case (-> ((:schema ctx#) (-> ctx# :attribute :attribute/ident)) :attribute/cardinality :db/ident)
                            :db.cardinality/one nil
                            :db.cardinality/many (hash (into #{} (mapv :db/id (:value ctx#))))))
                     hash js/Math.abs - str)))

      ;entity create
      (and (not r) (not a))
      (pr-str `(fn [ctx#]
                 (-> (str (-> ctx# :entity :db/id :id) "." ".")
                     hash js/Math.abs - str)))

      :else nil)))
