(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cuerdas.core :as str]
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment from-react-context]]
    [contrib.ui]
    [contrib.uri :refer [is-uri?]]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [browse field hyper-control link markdown]]
    ))


(defn schema-links [ctx]
  (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/environment])
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]
                           :target "_blank"}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))

(def underdocs
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs!fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs!fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs!fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs!fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs!fiddle-renderer/). `ctx` and
   `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs!fiddle-links/)."})

(def controls
  {:fiddle/pull (from-react-context
                  (fn [{:keys [ctx props]} value]
                    [:div
                     [(hyper-control ctx) value]
                     (fragment [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/pull underdocs)]])]))
   :fiddle/query (from-react-context
                   (fn [{:keys [ctx props]} value]
                     [:div
                      [(hyper-control ctx) value]
                      (fragment [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/query underdocs)]])]))
   :fiddle/markdown (from-react-context
                      (fn [{:keys [ctx props]} value]
                        [:div
                         [(hyper-control ctx) value]
                         [:div.hf-underdoc [markdown (:fiddle/markdown underdocs)]]]))
   :fiddle/css (from-react-context
                 (fn [{:keys [ctx props]} value]
                   [:div
                    [(hyper-control ctx) value]
                    [:div.hf-underdoc [markdown (:fiddle/css underdocs)]]]))
   :fiddle/renderer (from-react-context
                      (fn [{:keys [ctx props]} value]
                        [:div
                         [(hyper-control ctx) value]
                         [:div.hf-underdoc [markdown (:fiddle/renderer underdocs)]]]))
   :fiddle/links (from-react-context
                   (fn [{:keys [ctx props]} value]
                     [:div
                      [(hyper-control ctx) value]
                      [:div.hf-underdoc [markdown (:fiddle/links underdocs)]]]))
   })

(defn fiddle-src-renderer [ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)]
    [:div {:class class}
     [:h3 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]
     (field [0 :fiddle/ident] ctx nil)
     (field [0 :fiddle/type] ctx nil)
     (case @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/type])
       :entity (fragment (field [0 :fiddle/pull-database] ctx nil)
                         (field [0 :fiddle/pull] ctx (controls :fiddle/pull)))
       :query (field [0 :fiddle/query] ctx (controls :fiddle/query))
       :blank nil
       nil nil)
     (field [0 :fiddle/markdown] ctx (controls :fiddle/markdown))
     (field [0 :fiddle/css] ctx (controls :fiddle/css))
     (field [0 :fiddle/renderer] ctx (controls :fiddle/renderer))
     (field [0 :fiddle/hydrate-result-as-fiddle] ctx nil)
     (when-not embed-mode (field [0 :fiddle/links] ctx-real (controls :fiddle/links)))
     (when-not embed-mode (link :hyperfiddle/remove [0] ctx "Remove fiddle"))]))

(defn ^:deprecated hyperfiddle-live [rel ctx & fiddle-attrs]
  (let [state (r/atom {:edn-fiddle false :edn-result false})]
    (fn [rel ctx & fiddle-attrs]
      [:div.row.hf-live.unp.no-gutters
       (let [as-edn (r/cursor state [:edn-result])]
         [:div.result.col-sm.order-sm-2.order-xs-1
          [:div "Result:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          (browse rel [] ctx (if @as-edn (r/partial hyperfiddle.ide.hf-live/result-edn [])))])
       (let [as-edn (r/cursor state [:edn-fiddle])
             f (r/partial (if @as-edn hyperfiddle.ide.hf-live/result-edn hyperfiddle.ide.hf-live/docs-embed) fiddle-attrs)]
         [:div.src.col-sm.order-sm-1.order-xs-2
          [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox-boolean " EDN?" as-edn {:class "hf-live"}]]
          (browse rel [] ctx f {:frag ":src" :class "devsrc"})])
       ])))
