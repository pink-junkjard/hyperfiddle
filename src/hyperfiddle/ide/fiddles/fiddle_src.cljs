(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cuerdas.core :as str]
            [contrib.reactive :as r]
            [hypercrud.types.URI :refer [is-uri?]]
            [hypercrud.ui.auto-control :refer [auto-control']]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.form :refer [form-cell]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]))


(defn schema-links [ctx]
  (->> (-> ctx :target-domain :domain/environment)
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))

(defn control-with-unders [& children]
  (fn [field props ctx]
    (let [control [(auto-control' ctx) field props ctx]]
      (into [:div control] children))))

(defn cell-wrap [& [?control]]
  (fn [ctx]
    (let [control (or ?control (auto-control' ctx))]
      [form-cell control (:hypercrud.browser/field ctx) ctx])))

(defn fiddle-src-renderer [ctx-real class]
  #_(hypercrud.ui.result/fiddle ctx-real)
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx' (shadow-fiddle ctx-real)
        rtype (:fiddle/type @(:hypercrud.browser/result ctx'))]
    [:div {:class (str class " fiddle-src")}
     [:h3 "fiddle src"]
     ((:cell ctx') [true 0 :fiddle/ident] ctx')
     ((:cell ctx') [true 0 :db/doc] ctx')
     ((:cell ctx') [true 0 :fiddle/type] ctx')
     (case rtype
       :entity ((:cell ctx') [true 0 :fiddle/pull] ctx'
                 (cell-wrap
                   (control-with-unders
                     [:span.schema "schema: " (schema-links ctx')]
                     [markdown "`:db/id` is currently required. No nested pull yet. Just keep it simple for now. See
                     [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."])))
       :query ((:cell ctx') [true 0 :fiddle/query] ctx'
                (cell-wrap
                  (control-with-unders
                    [:span.schema "schema: " (schema-links ctx')]
                    [markdown "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs!fiddle-query/) and
                    the [Datomic query docs](https://docs.datomic.com/on-prem/query.html). If you pull, `:db/id` is
                    required. No rules yet, no nested pull yet, no d/log or d/history yet."])))
       :blank nil
       nil nil)
     ((:cell ctx') [true 0 :fiddle/markdown] ctx' (cell-wrap (control-with-unders [markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs!fiddle-markdown/)."])))
     ((:cell ctx') [true 0 :fiddle/renderer] ctx' (cell-wrap (control-with-unders [markdown "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs!fiddle-renderer/)."])))
     ((:cell ctx') [true 0 :fiddle/css] ctx' (cell-wrap (control-with-unders [markdown "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs!fiddle-css/)."])))
     ((:cell ctx') [true 0 :fiddle/links] ctx-real (cell-wrap (control-with-unders [markdown "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs!fiddle-links/)."])))
     ((:cell ctx') [true 0 :fiddle/entrypoint?] ctx')
     ;((:cell ctx') [true 0 :fiddle/bindings] ctx')
     ((:anchor ctx') :hyperfiddle/remove [0] ctx' "Remove fiddle")
     ((:browse ctx-real) :attribute-renderers [] ctx-real)
     ]))
