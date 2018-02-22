(ns hypercrud.ui.result
  (:require [cats.monad.either :as either]
            [datascript.parser :as parser]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]
            [hypercrud.util.core :as util]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]))


(defn result-renderer [result ordered-fes links ctx]
  (let [ctx (assoc ctx :ordered-fes ordered-fes
                       :links links)]
    ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
    ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
    ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
    (case (get-in ctx [:fiddle :fiddle/type])
      :entity (if-let [a (get-in ctx [:request :a])]
                (let [[e a] (get-in ctx [:route :request-params])]
                  (either/branch
                    (try-either (.-dbname e))
                    (fn [e]
                      [:pre (util/pprint-str e)])
                    (fn [source-symbol]
                      (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                        :db.cardinality/one
                        (form/Relation (context/relation ctx (reactive/atom [result])))

                        :db.cardinality/many
                        (table/Table (context/relations ctx (mapv vector result)))))))
                (form/Relation (context/relation ctx (reactive/atom [result]))))

      :query (either/branch
               (try-either (parser/parse-query (get-in ctx [:request :query])))
               (fn [e]
                 [:pre (util/pprint-str e)])
               (fn [{:keys [qfind]}]
                 (condp = (type qfind)
                   datascript.parser.FindRel (table/Table (context/relations ctx (mapv vec result)))
                   datascript.parser.FindColl (table/Table (context/relations ctx (mapv vector result)))
                   datascript.parser.FindTuple (form/Relation (context/relation ctx (reactive/atom (vec result))))
                   datascript.parser.FindScalar (form/Relation (context/relation ctx (reactive/atom [result]))))))

      :blank nil

      nil)))

(defn view [result ordered-fes links ctx]
  ;(:anchors ctx)
  (let [{index-inline-links true index-links false} (->> (link/links-lookup' links [])
                                                         (remove :link/dependent?) ; link/dependent? true = relation link
                                                         (group-by #(or (:link/render-inline? %) false)))
        index-ctx (dissoc ctx :isComponent)]
    [:div.auto-result
     [:div.hyperfiddle-fiddle-doc (markdown-rendered* (-> ctx :fiddle :db/doc))]
     (link-controls/render-links index-links index-ctx)
     (result-renderer result ordered-fes links ctx)
     (link-controls/render-inline-links index-inline-links index-ctx)]))
