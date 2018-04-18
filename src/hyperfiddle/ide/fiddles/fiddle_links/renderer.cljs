(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require [cats.monad.either :as either]
            [contrib.reactive :as reactive]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.ui.error :as ui-error]
            [hypercrud.ui.result :as result]))

(defn links->result [links]
  (->> @links
       ;(sort-by (juxt :link/disabled :link/rel)
       ;         (fn [[a b] [a' b']] (if (= a a') (< b b') (< a a'))))
       (mapv (fn [link]
               (if (system-fiddle/system-fiddle? (get-in link [:link/fiddle :db/ident]))
                 (dissoc link :link/fiddle)
                 link)))))

(defn renderer [ctx]
  (-> (base/data-from-route (:target-route ctx)
                            (assoc ctx
                              :hypercrud.browser/domain (:target-domain ctx)
                              :keep-disabled-anchors? true))
      (either/branch
        (fn [e]
          [:div
           [(ui-error/error-comp ctx) e]
           (result/fiddle ctx)])
        (fn [{:keys [:hypercrud.browser/links]}]
          (result/result
            (-> ctx
                (dissoc :relation :relations)
                (assoc :hypercrud.browser/result (reactive/track links->result links))
                context/with-relations))))))
