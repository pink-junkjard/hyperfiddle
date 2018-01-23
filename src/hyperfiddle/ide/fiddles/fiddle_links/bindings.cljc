(ns hyperfiddle.ide.fiddles.fiddle-links.bindings
  (:require [hypercrud.browser.auto-anchor :as auto-anchor]))


(let [always-readonly (constantly true)
      never-readonly (constantly false)
      field (fn [control maybe-field ctx]
              (let [sys? (auto-anchor/system-anchor? (get-in ctx [:cell-data :db/id]))
                    shadow? (get-in ctx [:cell-data :hypercrud/sys?])
                    cantchange (contains? #{:link/rel
                                            :link/path
                                            :link/create?
                                            :link/managed?
                                            :link/dependent?}
                                          (-> ctx :attribute :db/ident))
                    readonly (or sys? (and shadow? cantchange))
                    ctx (assoc ctx :read-only (if readonly always-readonly never-readonly))]
                ;(table/Field control maybe-field ctx)
                ))]
  (defn bindings [ctx]
    ctx
    ;(assoc ctx :field field)
    ))
