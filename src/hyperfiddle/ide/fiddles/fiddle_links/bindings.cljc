(ns hyperfiddle.ide.fiddles.fiddle-links.bindings
  (:require [contrib.reactive :as reactive]
            [hypercrud.browser.system-link :refer [system-link?]]))


(let [read-only (fn [attribute ctx]
                  (let [sys? (system-link? @(reactive/cursor (:cell-data ctx) [:db/id]))
                        shadow? @(reactive/cursor (:cell-data ctx) [:hypercrud/sys?])
                        cantchange (contains? #{:link/rel
                                                :link/path
                                                :link/create?
                                                :link/managed?
                                                :link/dependent?}
                                              (:db/ident attribute))]
                    (or sys? (and shadow? cantchange))))]
  (defn bindings [ctx]
    (assoc ctx :read-only read-only)))
