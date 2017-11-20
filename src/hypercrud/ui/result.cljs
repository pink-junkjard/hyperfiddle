(ns hypercrud.ui.result
  (:require [cats.monad.either :as either :refer-macros [try-either]]
            [datascript.parser :as parser]
            [hypercrud.browser.base :as base]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.markdown :as markdown]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.widget :as widget]
            [hypercrud.util.core :as util]))

; todo add these fns back
; todo is this layout actually unique to forms? or can this be reused for tables/blank?
(defn form-class-fn [ctx]
  (str "forms-list " (name (:layout ctx))))

(defn result-renderer [result ordered-fes anchors-lookup ctx]
  (case (get-in ctx [:fiddle :request/type])
    :entity (if-let [a (get-in ctx [:request :a])]
              (either/branch
                (try-either (.-dbname (get-in ctx [:route :request-params :entity])))
                (fn [e]
                  [:pre (util/pprint-str e)])
                (fn [source-symbol]
                  (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                    :db.cardinality/one
                    (form/Relation [result] ordered-fes anchors-lookup ctx)

                    :db.cardinality/many
                    [table/Table (map vector result) ordered-fes anchors-lookup ctx])))
              (form/Relation [result] ordered-fes anchors-lookup ctx))

    :query (either/branch
             (parser/parse-query (get-in ctx [:request :query]))
             (fn [e]
               [:pre (util/pprint-str e)])
             (fn [{:keys [qfind]}]
               (condp = (type qfind)
                 datascript.parser.FindRel [table/Table result ordered-fes anchors-lookup ctx]
                 datascript.parser.FindColl [table/Table (map vector result) ordered-fes anchors-lookup ctx]
                 datascript.parser.FindTuple (form/Relation result ordered-fes anchors-lookup ctx)
                 datascript.parser.FindScalar (form/Relation [result] ordered-fes anchors-lookup ctx))))

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
