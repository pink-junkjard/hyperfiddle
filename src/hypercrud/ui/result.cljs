(ns hypercrud.ui.result
  (:require [cats.monad.either :as either :refer-macros [try-either]]
            [datascript.parser :as parser]
            [hypercrud.browser.base :as base]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-rendered*]]
            [hypercrud.ui.table :as table]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.util.core :as util]
            [hypercrud.browser.link :as link]))


(defn result-renderer [result ordered-fes links ctx]
  ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
  ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
  ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
  (case (get-in ctx [:fiddle :fiddle/type])
    :entity (if-let [a (get-in ctx [:request :a])]
              (either/branch
                (try-either (.-dbname (get-in ctx [:route :request-params :entity])))
                (fn [e]
                  [:pre (util/pprint-str e)])
                (fn [source-symbol]
                  (case (get-in ctx [:schemas (str source-symbol) a :db/cardinality :db/ident])
                    :db.cardinality/one
                    (form/Relation [result] ordered-fes links ctx)

                    :db.cardinality/many
                    (table/Table (map vector result) ordered-fes links ctx))))
              (form/Relation [result] ordered-fes links ctx))

    :query (either/branch
             (try-either (parser/parse-query (get-in ctx [:request :query])))
             (fn [e]
               [:pre (util/pprint-str e)])
             (fn [{:keys [qfind]}]
               (condp = (type qfind)
                 datascript.parser.FindRel (table/Table result ordered-fes links ctx)
                 datascript.parser.FindColl (table/Table (map vector result) ordered-fes links ctx)
                 datascript.parser.FindTuple (form/Relation result ordered-fes links ctx)
                 datascript.parser.FindScalar (form/Relation [result] ordered-fes links ctx))))

    :blank nil

    nil))

(defn view [result ordered-fes links ctx]
  ;(:anchors ctx)
  (let [{index-inline-links true index-links false} (->> (link/links-lookup' links [])
                                                         (remove :link/dependent?) ; link/dependent? true = relation link
                                                         (group-by :link/render-inline?))
        index-ctx (dissoc ctx :isComponent)]
    [:div.auto-result
     [:div.hyperfiddle-fiddle-doc (markdown-rendered* (str (-> ctx :fiddle :db/doc)))]
     (link-controls/render-links index-links index-ctx)
     (result-renderer result ordered-fes links ctx)
     (link-controls/render-inline-links index-inline-links index-ctx)]))
