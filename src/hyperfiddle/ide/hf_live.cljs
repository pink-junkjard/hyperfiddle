(ns hyperfiddle.ide.hf-live
  (:require
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment from-react-context]]
    [contrib.ui]
    [hypercrud.browser.router :as router]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ui :refer [browse field hyper-control link markdown]]
    [hypercrud.browser.browser-ui :refer [ui-comp]]
    ))


(defn hacked-links [value]
  (let [links (->> value
                   (remove :link/disabled?)
                   (map #(select-keys % [:link/rel :link/path :link/fiddle :link/render-inline? :link/formula]))
                   (map #(update % :link/fiddle :fiddle/ident)))]
    [:div
     [:pre (if (seq links)
             (with-out-str (clojure.pprint/print-table links))
             "No links or only auto links")]
     [:div.hf-underdoc [markdown "Links are not editable for now due to an issue"]]]))

(def controls
  {:fiddle/pull (from-react-context
                  (fn [{:keys [ctx props]} value]
                    [:div
                     [(hyper-control ctx) value]
                     [:span.schema "schema: " (fiddle-src/schema-links ctx)]]))

   :fiddle/query (from-react-context
                   (fn [{:keys [ctx props]} value]
                     [:div
                      [(hyper-control ctx) value]
                      [:span.schema "schema: " (fiddle-src/schema-links ctx)]]))

   :fiddle/links hacked-links})

(defn docs-embed [attrs ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)
        {:keys [:fiddle/ident]} @(:hypercrud.browser/result ctx)]
    (fn [ctx-real class & {:keys [embed-mode]}]
      (into
        [:div {:class class}
         #_[:h5 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]]
        (for [k attrs]
          (field [0 k] ctx (controls k)))))))

(defn result-edn [attrs {:keys [hypercrud.browser/result]}]
  (let [s (-> @result
              (as-> $ (if (seq attrs) (select-keys $ attrs) $))
              hyperfiddle.ui.hacks/pull-soup->tree
              (contrib.pprint/pprint-str 40))]
    [contrib.ui/code s #() {:read-only true}]))

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
                      (dissoc ctx :user-renderer)) #_"infinite recursion somehow"]
            [ui-comp ctx (str class " hf-live")])])
       (let [as-edn (r/cursor state [:edn-fiddle])
             f (r/partial (if @as-edn result-edn docs-embed) fiddle-attrs)]
         [:div.src.col-sm.order-sm-1.order-xs-2
          [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          [ui-comp (assoc ctx :route (router/assoc-frag (:route ctx) ":src") :user-renderer f) (css class "devsrc hf-live")]])
       ])))
