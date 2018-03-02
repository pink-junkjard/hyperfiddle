(ns hypercrud.ui.result
  (:require [cats.monad.either :as either]
            [hypercrud.browser.result :as result]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.util.core :as util]
            [hypercrud.util.non-fatal :refer [try-either]]))


(defn result-renderer [ctx]
  ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
  ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
  ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
  (-> (case (get-in ctx [:fiddle :fiddle/type])
        :entity (result/with-entity-relations ctx
                                              :entity #(form/Relation % ctx)
                                              :attr-one #(form/Relation % ctx)
                                              :attr-many #(table/Table % ctx))
        :query (result/with-query-relations ctx
                                            :relation #(form/Relation % ctx)
                                            :relations #(table/Table % ctx))
        :blank (either/right nil)
        (either/right nil))
      (either/branch
        (fn [e] [:pre (util/pprint-str e)])
        identity)))

(defn view [ctx]
  (let [index-ctx (dissoc ctx :isComponent)]
    [:div.auto-result
     [:div.hyperfiddle-fiddle-doc (markdown-rendered* (-> ctx :fiddle :db/doc))]
     (link-controls/render-nav-cmps [] false index-ctx)
     (result-renderer ctx)
     (link-controls/render-inline-links [] false index-ctx)]))
