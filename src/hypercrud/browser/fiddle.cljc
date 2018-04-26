(ns hypercrud.browser.fiddle
  (:require
    [contrib.string :refer [or-str]]))


(defn fiddle-defaults [fiddle]
  ; Don't call on syslinks, shadow pull crashes the fiddle dunno why
  (merge-with or-str fiddle
              {:fiddle/markdown "!result[]"
               :fiddle/renderer #?(:clj nil :cljs (-> hypercrud.ui.result/fiddle meta :expr-str))
               :fiddle/pull "[[:db/id *]]"
               :fiddle/query "[:find (pull ?e [:db/id *]) :where\n [?e :db/ident :db/add]]"}))
