(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.ui.result :as result]
            [contrib.reactive :as reactive]))

(defn links->result [links]
  (->> @links
       ;(sort-by (juxt :link/disabled :link/rel)
       ;         (fn [[a b] [a' b']] (if (= a a') (< b b') (< a a'))))
       (mapv (fn [link]
               (-> (if (system-fiddle/system-fiddle? (get-in link [:link/fiddle :db/ident]))
                     (dissoc link :link/fiddle)
                     link)
                   (update :link/formula #(or (-> % meta :str) %))
                   (update :link/tx-fn #(or (-> % meta :str) %)))))))

(defn renderer [ctx]
  (-> (base/data-from-route (:target-route ctx)
                            (assoc ctx
                              :hypercrud.browser/domain (:target-domain ctx)
                              :keep-disabled-anchors? true))
      (either/branch
        (fn [e]
          [:div
           (browser-ui/ui-error e ctx)
           (result/view ctx)])
        (fn [{:keys [:hypercrud.browser/links]}]
          (result/view (assoc ctx :hypercrud.browser/result (reactive/track links->result links)))))))
