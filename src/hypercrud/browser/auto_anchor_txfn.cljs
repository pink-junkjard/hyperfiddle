(ns hypercrud.browser.auto-anchor-txfn
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.client.tx]
            [hypercrud.compile.macros :refer-macros [str-and-code]]
            [hypercrud.types.DbId :refer [->DbId]]
            [hypercrud.util.core :refer [pprint-str]]))


(defn auto-txfn [anchor]
  (let [{m :anchor/managed? fe :anchor/find-element a :anchor/attribute} anchor]
    (cond
      (and m fe a)                                          ; attr create
      (str-and-code
        (fn [ctx get-tx-from-modal]
          (let [new-dbid (hypercrud.browser.auto-anchor-formula/auto-entity-dbid ctx)]
            {:tx (concat
                   (get-tx-from-modal)
                   (hypercrud.client.tx/edit-entity
                     (-> ctx :entity :db/id)
                     (-> ctx :attribute :attribute/ident)
                     []
                     [new-dbid]))})))

      (and m fe (not a))
      (str-and-code
        (fn [ctx get-tx-from-modal]
          (let [branch (hypercrud.browser.auto-anchor-formula/auto-entity-dbid ctx)
                id' (-> (js/Date.now) - str)
                ; alter the dbid before transacting so it can be reused.
                ; This has to happen at a side-effect point and will cause a hydrate
                ; so we do it when we already have to hydrate.
                tx-from-modal' (->> (get-tx-from-modal)
                                    (mapv (fn [[op dbid a v]]
                                            (if (= (:id dbid) branch)
                                              [op (hypercrud.types.DbId/->DbId id' (:conn-id dbid)) a v]
                                              [op dbid a v]))))]
            {:tx tx-from-modal'})))

      :else nil)))
