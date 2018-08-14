(ns hyperfiddle.ide.hf-live
  (:require
    [contrib.css :refer [css]]
    [contrib.data :refer [compare-by-index]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.browser.browser-ui :refer [ui-comp]]
    [hypercrud.browser.router :as router]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ui :refer [fiddle-api field]]))

; This is an entity in the project namespace in the IDE fiddle-repo, probably
(def attr-order [:fiddle/ident :fiddle/type :fiddle/pull-database :fiddle/pull :fiddle/query
                 :fiddle/renderer :fiddle/css :fiddle/markdown :fiddle/links :fiddle/hydrate-result-as-fiddle])

(defn docs-embed [attrs ctx-real class & {:keys [embed-mode]}]
  (let [attrs (or (seq attrs)
                  (clojure.set/intersection
                    (-> @(:hypercrud.browser/data ctx-real)
                        keys
                        (->> (apply sorted-set-by (compare-by-index attr-order)))
                        (disj :db/id))))
        ctx (shadow-fiddle ctx-real)]
    (fn [ctx-real class & {:keys [embed-mode]}]
      (into
        [:div {:class class}]
        (for [k attrs
              :let [?f (fiddle-src/controls k)
                    props (when ?f {:embed-mode true})]]
          (field [0 k] ctx ?f props))))))

(defn result-edn [attrs ctx class]
  (let [s (-> @(:hypercrud.browser/data ctx)
              (as-> $ (if (seq attrs) (select-keys $ attrs) $)) ; omit elided fiddle attrs
              hyperfiddle.ui.hacks/pull-soup->tree
              (contrib.pprint/pprint-str 50))]
    [contrib.ui/code s #() {:read-only true
                            :class class #_"Class ends up not on the codemirror, todo"}]))

; This is in source control because all hyperblogs want it.
; But, they also want to tweak it surely. Can we store this with the fiddle ontology?
(defn ^:export hf-live [& fiddle-attrs]
  (let [state (r/atom {:edn-fiddle false :edn-result false})]
    (fn [ctx class]
      [:div.row.hf-live.unp.no-gutters
       (let [as-edn (r/cursor state [:edn-result])
             f (when @as-edn fiddle-api)]
         [:div.result.col-sm.order-sm-2.order-xs-1
          [:div "Result:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          ; Careful: Reagent deep bug in prop comparison https://github.com/hyperfiddle/hyperfiddle/issues/340
          (let [ctx (if f
                      ctx
                      (dissoc ctx :hyperfiddle.ui/unp))]
            [ui-comp ctx {:class (str class " hf-live")
                          :user-renderer f}])])
       (let [as-edn (r/cursor state [:edn-fiddle])
             f (r/partial (if @as-edn result-edn docs-embed) fiddle-attrs)]
         [:div.src.col-sm.order-sm-1.order-xs-2
          [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          [ui-comp (assoc ctx :route (router/assoc-frag (:route ctx) ":src"))
           {:class (css class "devsrc hf-live")
            :user-renderer f}]])
       ])))
