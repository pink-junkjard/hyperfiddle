(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require [cats.core :as cats]
            [cuerdas.core :as str]
            [contrib.data :refer [kwargs unwrap]]
            [contrib.reagent :refer [fragment]]
            [contrib.string :refer [safe-read-edn-string]]
            [hypercrud.types.URI :refer [is-uri?]]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.tooltip :refer [tooltip]]
            [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]))


(defn fiddle-src-renderer [ctx-real class]
  #_(hypercrud.ui.result/fiddle ctx-real)
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx' (shadow-fiddle ctx-real)
        rtype (:fiddle/type @(:hypercrud.browser/result ctx'))]
    [:div {:class class}
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


(defn ^:export fiddle-query-renderer [field props ctx]
  [:div
   [hypercrud.ui.attribute.code/code field props ctx]
   (->> (-> ctx :target-domain :domain/environment)
        (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
        (map (fn [[$db _]]
               (let [props {:route [(keyword "hyperfiddle.schema" $db)]}]
                 ^{:key $db}
                 [(:navigate-cmp ctx) props (str $db " schema")])))
        (doall))])
