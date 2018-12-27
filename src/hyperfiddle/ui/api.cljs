(ns hyperfiddle.ui.api
  (:require
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hyperfiddle.api]
    [hyperfiddle.data]))


(defn with-result [ctx]
  (for [ctx (hyperfiddle.api/spread-fiddle ctx)]
    @(:hypercrud.browser/data ctx)))

(defn api-data "{route data}
  At this point we only care about inline links
  also no popovers can be opened, so remove managed"
  [ctx]
  (merge

    {@(:hypercrud.browser/route ctx) (with-result ctx)}

    (->> @(hyperfiddle.data/select-many-here ctx #{:hf/iframe}) ; this omits dependent iframes fixme
         (map (partial r/flip base/data-from-link! ctx))
         (map (juxt :hypercrud.browser/route with-result)))

    (when @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
      (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)
            route [inner-fiddle (vec inner-args)]
            ctx (base/data-from-route! route ctx)]
        ((juxt :hypercrud.browser/route with-result) ctx)))))
