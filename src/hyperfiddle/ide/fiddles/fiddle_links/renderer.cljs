(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require [cats.monad.either :as either]
            [contrib.reactive :as r]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
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

(defn renderer [ctx class]
  (-> (base/data-from-route (:target-route ctx)
                            (assoc ctx
                              :hypercrud.browser/domain (:target-domain ctx)
                              :keep-disabled-anchors? true))
      (either/branch
        (fn [e]
          [:div {:class class}
           [(ui-error/error-comp ctx) e]
           (result/fiddle ctx)])
        (fn [{:keys [:hypercrud.browser/links]}]
          (let [ctx (-> ctx
                        (dissoc :relation :relations)
                        (assoc :hypercrud.browser/result (r/track links->result links))
                        context/with-relations)]
            [:div {:class class}
             [markdown (some-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]])))))
