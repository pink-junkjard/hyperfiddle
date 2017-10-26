(ns hypercrud.ui.result
  (:require [hypercrud.ui.form :as form]
            [hypercrud.ui.markdown :as markdown]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.widget :as widget]))


(defn view [result ordered-fes anchors param-ctx]
  [:div.auto-result
   [markdown/markdown (str (-> param-ctx :fiddle :db/doc)) #() {:class "hypercrud-doc"}]
   (cond
     (map? result) [form/form result ordered-fes anchors param-ctx]
     (coll? result) [table/ui-table result ordered-fes anchors param-ctx]
     :else
     [:div.blank
      (widget/render-anchors (->> anchors
                                  (remove :anchor/find-element)
                                  (remove :anchor/attribute)
                                  (remove :anchor/render-inline?))
                             param-ctx)
      (widget/render-inline-anchors (->> anchors
                                         (remove :anchor/find-element)
                                         (remove :anchor/attribute)
                                         (filter :anchor/render-inline?))
                                    (dissoc param-ctx :isComponent))])])
