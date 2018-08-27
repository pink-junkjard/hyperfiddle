(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.system-fiddle :as system-fiddle]
    [hypercrud.types.Entity :refer [->Entity shadow-entity]]
    [hypercrud.ui.error :as error]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ide.fiddles.fiddle-links.renderer :as links-fiddle]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link markdown]]))


(defn process-links [uri links]
  (->> links
       ;(sort-by (juxt :link/disabled :link/rel)
       ;         (fn [[a b] [a' b']] (if (= a a') (< b b') (< a a'))))
       (mapv (fn [link]
               (->> (if (system-fiddle/system-fiddle? (get-in link [:link/fiddle :db/ident]))
                      (dissoc link :link/fiddle)
                      link)
                    (->Entity uri))))))

(defn inject-links [fiddle-ref links-ref]
  (let [fiddle @fiddle-ref]
    (shadow-entity fiddle #(assoc % :fiddle/links (process-links (some-> fiddle .-uri) @links-ref)))))

; todo does this merge with shadow-fiddle?
; not sure if other downstream consumers of shadow-fiddle can run data-from-route
(defn shadow-links [ctx]
  (->> (base/data-from-route (context/target-route ctx)
                             (assoc ctx
                               :hypercrud.browser/domain @(runtime/state (:peer ctx) [::runtime/domain])
                               :keep-disabled-anchors? true))
       (cats/fmap (fn [{:keys [:hypercrud.browser/links]}]
                    (update ctx :hypercrud.browser/data #(r/track inject-links % links))))))

(defn schema-links [ctx]
  (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/databases])
       (map (juxt :domain.database/name (comp :database/uri :domain.database/record)))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]
                           :target "_blank"}]
                ^{:key $db}
                [anchor ctx props $db])))
       (doall)))

(def underdocs
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs/:fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs/:fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs/:fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs/:fiddle-css/)."
   :fiddle/renderer "`ctx` and `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified. See [:fiddle/renderer docs](http://www.hyperfiddle.net/:docs/:fiddle-renderer/). "
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs/:fiddle-links/)."})

(def controls
  {:fiddle/pull (fn [val ctx props]
                  [:div
                   [hyper-control val ctx (dissoc props :embed-mode)]
                   [:span.schema "schema: " (schema-links ctx)]
                   (when-not (:embed-mode props)
                     #_[:div.hf-underdoc [markdown (:fiddle/pull underdocs)]])])
   :fiddle/query (fn [val ctx props]
                   [:div
                    [hyper-control val ctx (dissoc props :embed-mode)]
                    [:span.schema "schema: " (schema-links ctx)]
                    (when-not (:embed-mode props)
                      #_[:div.hf-underdoc [markdown (:fiddle/query underdocs)]])])
   :fiddle/markdown (fn [val ctx props]
                      [:div
                       [hyper-control val ctx (dissoc props :embed-mode)]
                       (when-not (:embed-mode props)
                         #_[:div.hf-underdoc [markdown (:fiddle/markdown underdocs)]])])
   :fiddle/css (fn [val ctx props]
                 [:div
                  [hyper-control val ctx (dissoc props :embed-mode)]
                  (when-not (:embed-mode props)
                    #_[:div.hf-underdoc [markdown (:fiddle/css underdocs)]])])
   :fiddle/renderer (fn [val ctx props]
                      [:div
                       [hyper-control val ctx (dissoc props :embed-mode)]
                       (when-not (:embed-mode props)
                         [:div.hf-underdoc [markdown (:fiddle/renderer underdocs)]])])
   :fiddle/links (fn [val ctx props]
                   [:div
                    (link :hyperfiddle/new "link" ctx)
                    [:div [links-fiddle/renderer val ctx props]]
                    (when-not (:embed-mode props)
                      #_[:div.hf-underdoc [markdown (:fiddle/links underdocs)]])])
   })

(defn fiddle-src-renderer [val ctx props]
  (let [ctx (shadow-fiddle ctx)
        ; these two shadow calls are inefficient, throwing away work
        ectx (shadow-links ctx)
        ctx (either/branch ectx (constantly ctx) identity)]
    [:div props
     [:h3 (str @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/ident])) " source"]
     (field [:fiddle/ident] ctx nil)
     (field [:fiddle/type] ctx nil)
     (case @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/type])
       :entity (fragment (field [:fiddle/pull-database] ctx nil)
                         (field [:fiddle/pull] ctx (controls :fiddle/pull)))
       :query (field [:fiddle/query] ctx (controls :fiddle/query))
       :blank nil
       nil nil)
     (field [:fiddle/renderer] ctx (controls :fiddle/renderer))
     (field [:fiddle/css] ctx (controls :fiddle/css))
     (field [:fiddle/markdown] ctx (controls :fiddle/markdown))
     (when (either/left? ectx)
       [:div
        [:h5 "Unable to shadow links:"]
        [error/error-block @ectx]])
     (field [:fiddle/links] ctx (controls :fiddle/links))
     (field [:fiddle/hydrate-result-as-fiddle] ctx nil)
     [:div.p "Additional attributes"]
     (->> @(r/fmap ::field/children (:hypercrud.browser/field ctx))
          ; todo tighter reactivity
          (map ::field/path-segment)
          (remove #(= (namespace %) "fiddle"))
          (map #(field [%] ctx nil))
          (doall))
     (link :hyperfiddle/remove "fiddle" ctx "Remove fiddle" {:class "btn-outline-danger"})]))
