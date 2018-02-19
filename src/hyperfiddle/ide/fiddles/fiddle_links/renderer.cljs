(ns hyperfiddle.ide.fiddles.fiddle-links.renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.auto-fiddle :as auto-fiddle]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.ui.result :as result]
            [hyperfiddle.foundation :as foundation]))


(defn renderer [result ordered-fes links ctx]
  (-> (base/data-from-route (:target-route ctx)
                            (assoc ctx
                              :hypercrud.browser/domain (:target-domain ctx)
                              :keep-disabled-anchors? true))
      (either/branch
        (fn [e]
          [:div
           (browser-ui/ui-error e ctx)
           (result/view result ordered-fes links ctx)])
        (fn [data]
          (let [result (->> (:links data)
                            ;(sort-by (juxt :link/disabled :link/rel)
                            ;         (fn [[a b] [a' b']] (if (= a a') (< b b') (< a a'))))
                            (mapv (fn [link]
                                    (-> (if (auto-fiddle/system-fiddle? (get-in link [:link/fiddle :db/id]))
                                          (dissoc link :link/fiddle)
                                          link)
                                        (update :link/formula #(or (-> % meta :str) %))
                                        (update :link/tx-fn #(or (-> % meta :str) %))))))]
            (result/view result ordered-fes links ctx))))))
