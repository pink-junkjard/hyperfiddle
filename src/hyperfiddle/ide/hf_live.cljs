(ns hyperfiddle.ide.hf-live
  (:require
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.ui]
    [hypercrud.browser.browser-ui :refer [ui-comp]]
    [hypercrud.browser.router :as router]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ui :refer [field]]))

(defn docs-embed [attrs ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)
        {:keys [:fiddle/ident]} @(:hypercrud.browser/result ctx)]
    (fn [ctx-real class & {:keys [embed-mode]}]
      (into
        [:div {:class class}
         #_[:h5 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]]
        (for [k attrs
              :let [?f (fiddle-src/controls k)
                    props (when ?f {:embed-mode true})]]
          (field [0 k] ctx ?f props))))))

(defn result-edn [attrs {:keys [hypercrud.browser/result]} class]
  (let [s (-> @result
              (as-> $ (if (seq attrs) (select-keys $ attrs) $))
              hyperfiddle.ui.hacks/pull-soup->tree
              (contrib.pprint/pprint-str 40))]
    [contrib.ui/code s #() {:read-only true
                            :class class #_"Class ends up not on the codemirror, todo"}]))

; This is in source control because all hyperblogs want it.
; But, they also want to tweak it surely. Can we store this with the fiddle ontology?
(defn ^:export hf-live [& fiddle-attrs]
  (let [state (r/atom {:edn-fiddle false :edn-result false})]
    (fn [ctx class]
      [:div.row.hf-live.unp.no-gutters
       (let [as-edn (r/cursor state [:edn-result])
             f (if @as-edn (r/partial result-edn []))]
         [:div.result.col-sm.order-sm-2.order-xs-1
          [:div "Result:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          ; Careful: Reagent deep bug in prop comparison https://github.com/hyperfiddle/hyperfiddle/issues/340
          (let [ctx (if f
                      (assoc ctx :user-renderer f)
                      (dissoc ctx :user-renderer #_"infinite recursion somehow"
                              :hyperfiddle.ui/unp))]
            [ui-comp ctx (str class " hf-live")])])
       (let [as-edn (r/cursor state [:edn-fiddle])
             f (r/partial (if @as-edn result-edn docs-embed) fiddle-attrs)]
         [:div.src.col-sm.order-sm-1.order-xs-2
          [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          [ui-comp (assoc ctx :route (router/assoc-frag (:route ctx) ":src") :user-renderer f) (css class "devsrc hf-live")]])
       ])))
