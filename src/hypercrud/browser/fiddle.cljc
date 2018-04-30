(ns hypercrud.browser.fiddle
  (:require
    [contrib.string :refer [or-str]]))


(defn data-defaults [fiddle]
  #_(when-not (map? fiddle)
    (println (pr-str fiddle)))
  (cond-> fiddle
    (= :query (:fiddle/type fiddle)) (update :fiddle/query or-str "[:find (pull ?e [:db/id *]) :where\n [?e :db/ident :db/add]]")
    (= :entity (:fiddle/type fiddle)) (update :fiddle/pull or-str "[[:db/id *]]")))

(defn fiddle-defaults [fiddle]
  (-> (data-defaults fiddle)
      (update :fiddle/markdown or-str "!result[]")
      (update :fiddle/renderer or-str #?(:clj nil :cljs (-> hypercrud.ui.result/fiddle meta :expr-str)))))
