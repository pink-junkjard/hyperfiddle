(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cuerdas.core :as str]
            [hypercrud.types.URI :refer [is-uri?]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]))


(defn fiddle-src-renderer [ctx-real class]
  #_(hypercrud.ui.result/fiddle ctx-real)
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx' (shadow-fiddle ctx-real)
        rtype (:fiddle/type @(:hypercrud.browser/result ctx'))]
    [:div {:class (str class " fiddle-src")}
     [:h3 "fiddle src"]
     ((:cell ctx') [true 0 :fiddle/ident] ctx')
     ((:cell ctx') [true 0 :fiddle/type] ctx')
     (case rtype
       :entity ((:cell ctx') [true 0 :fiddle/pull] ctx')
       :query ((:cell ctx') [true 0 :fiddle/query] ctx')
       :blank nil
       nil nil)
     ((:cell ctx') [true 0 :fiddle/markdown] ctx')
     ((:cell ctx') [true 0 :fiddle/renderer] ctx')
     ((:cell ctx') [true 0 :fiddle/css] ctx')
     ((:cell ctx') [true 0 :fiddle/links] ctx-real)
     ((:cell ctx') [true 0 :fiddle/entrypoint?] ctx')
     ;((:cell ctx') [true 0 :fiddle/bindings] ctx')
     ((:anchor ctx') :hyperfiddle/remove [0] ctx' "Remove fiddle")
     ((:browse ctx-real) :attribute-renderers [] ctx-real)
     ]))

(defn schema-links [ctx]
  (->> (-> ctx :target-domain :domain/environment)
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))


(defn ^:export fiddle-query-renderer [field props ctx]
  [:div
   [hypercrud.ui.attribute.code/code field props ctx]
   "schema: " (schema-links ctx)
   [markdown "See [example queries]() and the [Datomic query docs](https://docs.datomic.com/cloud/query/query-data-reference.html)"]])

(defn ^:export fiddle-pull [field props ctx]
  [:div
   [hypercrud.ui.attribute.code/code field props ctx]
   "schema: " (schema-links ctx)
   [markdown "See [Datomic pull docs](https://docs.datomic.com/on-prem/query.html)"]])
