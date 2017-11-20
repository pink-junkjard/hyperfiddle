(ns hypercrud.ui.result
  (:require [datascript.parser :as parser]
            [hypercrud.browser.base :as base]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.markdown :as markdown]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.widget :as widget]))

; todo add these fns back
; todo is this layout actually unique to forms? or can this be reused for tables/blank?
(defn form-class-fn [ctx]
  (str "forms-list " (name (:layout ctx))))

(defn ui-table [relations ordered-fes anchors-lookup ctx]
  (if (every? #(empty? (:fields %)) ordered-fes)
    ; todo this error message is applicable for forms too, this should be raised up to hc.ui.result/view
    [:div "Can't infer table structure - no resultset and blank form. Fix query or model a form."]
    [table/Table relations ordered-fes anchors-lookup ctx]))

(defn result-renderer [result ordered-fes anchors-lookup ctx]
  (case (get-in ctx [:fiddle :request/type])
    :entity (if-let [a (get-in ctx [:request :a])]
              (case (let [source-symbol (.-dbname (get-in ctx [:route :request-params :entity]))]
                      (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident]))
                :db.cardinality/one
                (form/Relation [result] ordered-fes anchors-lookup ctx)

                :db.cardinality/many
                [table/Table (map vector result) ordered-fes anchors-lookup ctx])
              (form/Relation [result] ordered-fes anchors-lookup ctx))

    :query (let [{:keys [qfind]} (parser/parse-query (get-in ctx [:request :query]))]
             (condp = (type qfind)
               datascript.parser.FindRel [table/Table result ordered-fes anchors-lookup ctx]
               datascript.parser.FindColl [table/Table (map vector result) ordered-fes anchors-lookup ctx]
               datascript.parser.FindTuple (form/Relation result ordered-fes anchors-lookup ctx)
               datascript.parser.FindScalar (form/Relation [result] ordered-fes anchors-lookup ctx)))

    :blank nil

    nil))

(defn get-conf [result ctx]
  (js/console.log "todo")
  (constantly nil))

(defn view [result ordered-fes anchors ctx]
  (let [class-fn (get-conf result ctx)
        anchors-lookup (->> anchors
                            (map widget/process-popover-anchor)
                            (base/build-pathed-anchors-lookup))
        {inline-index-anchors true index-anchors false} (->> (get anchors-lookup :links)
                                                             (remove :anchor/repeating?) ; anchor/repeating? true = relation link
                                                             (group-by :anchor/render-inline?))
        index-ctx (dissoc ctx :isComponent)]
    [:div.auto-result
     [markdown/markdown (str (-> ctx :fiddle :db/doc)) #() {:class "hypercrud-doc"}]
     [:div {:class (class-fn ctx)}
      (widget/render-anchors index-anchors index-ctx)
      (result-renderer result ordered-fes anchors-lookup ctx)
      (widget/render-inline-anchors inline-index-anchors index-ctx)]]))
