(ns hypercrud.ui.result
  (:require [hypercrud.browser.base :as base]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.markdown :as markdown]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.widget :as widget]))

(def form-conf
  ; todo is this layout actually unique to forms? or can this be reused for tables/blank?
  {:class-fn (fn [ctx]
               (str "forms-list " (name (:layout ctx))))
   :ctx-fn (fn [ctx]
             (assoc ctx :layout (:layout ctx :block)))
   :renderer form/Relation})

(def table-conf
  {:class-fn (constantly nil)
   :ctx-fn identity
   :renderer table/ui-table})

(def blank-conf
  {:class-fn (constantly nil)
   :ctx-fn identity
   :renderer (constantly nil)})

(defn view [result ordered-fes anchors ctx]
  (let [{:keys [class-fn ctx-fn renderer]} (cond
                                             (map? result) form-conf
                                             (coll? result) table-conf
                                             :else blank-conf)
        anchors-lookup (->> anchors
                            (map widget/process-popover-anchor)
                            (base/build-pathed-anchors-lookup))
        {inline-index-anchors true index-anchors false} (->> (get anchors-lookup :links)
                                                             (remove :anchor/repeating?) ; anchor/repeating? true = relation link
                                                             (group-by :anchor/render-inline?))
        ctx (ctx-fn ctx)
        index-ctx (dissoc ctx :isComponent)]
    [:div.auto-result
     [markdown/markdown (str (-> ctx :fiddle :db/doc)) #() {:class "hypercrud-doc"}]
     [:div {:class (class-fn ctx)}
      (widget/render-anchors index-anchors index-ctx)
      (renderer result ordered-fes anchors-lookup ctx)
      (widget/render-inline-anchors inline-index-anchors index-ctx)]]))
