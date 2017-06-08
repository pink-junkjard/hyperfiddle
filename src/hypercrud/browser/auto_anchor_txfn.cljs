(ns hypercrud.browser.auto-anchor-txfn
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.client.tx :as tx]
            [hypercrud.types.DbId :refer [->DbId]]))


(defn auto-txfn [anchor]
  (let [{r :anchor/repeating? e :anchor/find-element a :anchor/attribute ident :anchor/ident} anchor]
    (cond

      ; link-query's always have a find-element (e)
      ; link-entity's never do (which is semantically unsound since semantically they always do)

      (and (not r) a)                                       ; attr create
      (pr-str `(fn [ctx# tx-from-modal#]
                 (let [new-dbid# (auto-entity-dbid ctx#)]
                   {:tx (concat
                          tx-from-modal#
                          (tx/edit-entity (-> ctx# :entity :db/id)
                                          (-> ctx# :attribute :attribute/ident)
                                          []
                                          [new-dbid#]))})))

      (and (not r) e (not a))
      (pr-str `(fn [ctx# tx-from-modal#]
                 (let [branch# (-> (str (-> ctx# :entity :db/id :id) "." nil ".")
                                   hash js/Math.abs - str)
                       id'# (-> (js/Date.now) - str)
                       ; alter the dbid before transacting so it can be reused.
                       ; This has to happen at a side-effect point and will cause a hydrate
                       ; so we do it when we already have to hydrate.
                       tx-from-modal'# (->> tx-from-modal#
                                            (mapv (fn [[op# dbid# a# v#]]
                                                    (if (= (:id dbid#) branch#)
                                                      [op# (->DbId id'# (:conn-id dbid#)) a# v#]
                                                      [op# dbid# a# v#]))))]
                   {:tx tx-from-modal'#})))

      (and r (not a) (= ident :remove))
      (pr-str `(fn [ctx# tx-from-modal#]
                 {:tx [[:db.fn/retractEntity (-> ctx# :entity :db/id)]]}))


      :else nil)))
