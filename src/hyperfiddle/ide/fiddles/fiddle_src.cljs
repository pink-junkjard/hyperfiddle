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
    [hypercrud.browser.find-element :as field]
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
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs/:fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs/:fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs/:fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs/:fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs/:fiddle-renderer/). `ctx` and
   `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs/:fiddle-links/)."})

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
     (field [0 :fiddle/links] ctx (controls :fiddle/links))
     [:div.p "Additional attributes"]
     (->> @(:hypercrud.browser/fields ctx)
          first
          ::field/children
          (map ::field/path-segment)
          (remove #(= (namespace %) "fiddle"))
          (map #(field [0 %] ctx nil))
          (doall))
     (when-not embed-mode (link :hyperfiddle/remove [:body 0] ctx "Remove fiddle"))]))
