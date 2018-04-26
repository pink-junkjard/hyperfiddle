(ns hypercrud.browser.fiddle
  (:require
    [contrib.string :refer [or-str]]))


(defn fiddle-defaults [fiddle]
  ; Don't call on syslinks, shadow pull crashes the fiddle dunno why
  (merge-with or-str fiddle
              {:fiddle/markdown "!result[]"
               :fiddle/renderer #?(:cljs hypercrud.ui.result/fiddle :clj nil)
               :fiddle/pull "[[:db/id *]]"
               :fiddle/query "[:find (pull ?e [:db/id *]) :where\n [?e]]"}))
