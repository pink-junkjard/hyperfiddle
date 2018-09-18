(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui]
    [hypercrud.browser.field :as field]
    [hypercrud.ui.error :as error]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ide.fiddles.fiddle-links.renderer :as links-fiddle]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link markdown]]
    [re-com.tabs :refer [horizontal-tabs]]))


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

(def controls
  {:fiddle/pull (fn [val ctx props]
                  [:div
                   [hyper-control val ctx (dissoc props :embed-mode)]
                   [:span.schema "schema: " (schema-links ctx)]])
   :fiddle/query (fn [val ctx props]
                   [:div
                    [hyper-control val ctx (dissoc props :embed-mode)]
                    [:span.schema "schema: " (schema-links ctx)]])
   :fiddle/markdown (fn [val ctx props]
                      [:div
                       [hyper-control val ctx (dissoc props :embed-mode)]])
   :fiddle/css (fn [val ctx props]
                 [:div
                  [hyper-control val ctx (dissoc props :embed-mode)]])
   :fiddle/renderer (fn [val ctx props]
                      [:div
                       [hyper-control val ctx (dissoc props :embed-mode)]
                       ])
   :fiddle/links (fn [val ctx props]
                   (let [ctx (if (:embed-mode props)
                               (links-fiddle/inject-topnav-links+ ctx)
                               ctx)]
                     [:div
                      (if-not (:embed-mode props)           ; more work to be done to get this popover hydrating
                        (link :hf/affix :link ctx))
                      [:div [links-fiddle/renderer val ctx props]]]))
   })

; Design wise, we only want one codemirror per tab, and it will be full-height.

(def tabs
  {:query (fn [val ctx]
            (fragment
              :query
              (field [:fiddle/type] ctx nil)
              (case @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/type])
                :entity (fragment (field [:fiddle/pull-database] ctx nil)
                                  (field [:fiddle/pull] ctx (controls :fiddle/pull)))
                :query (field [:fiddle/query] ctx (controls :fiddle/query))
                :blank nil
                nil nil)))
   :links (fn [val ctx]
            (field [:fiddle/links] ctx (controls :fiddle/links)))
   :view (fn [val ctx]
           (fragment
             :view
             (field [:fiddle/markdown] ctx (controls :fiddle/markdown))
             (field [:fiddle/renderer] ctx (controls :fiddle/renderer))))
   :css (fn [val ctx]
          (field [:fiddle/css] ctx (controls :fiddle/css)))

   :fiddle (fn [val ctx]
             (fragment
               :fiddle
               (field [:fiddle/ident] ctx nil)
               (field [:fiddle/hydrate-result-as-fiddle] ctx nil)
               #_[:div.p "Additional attributes"]
               (->> @(r/fmap ::field/children (:hypercrud.browser/field ctx))
                    ; todo tighter reactivity
                    (map ::field/path-segment)
                    (remove #(= (namespace %) "fiddle"))
                    (remove #(= :db/id %))
                    (map (fn [segment]
                           ^{:key (str [segment])}
                           [field [segment] ctx nil]))
                    (doall))
               (link :hf/remove :fiddle ctx "Remove fiddle" {:class "btn-outline-danger"})))})

(defn fiddle-src-renderer [val ctx props]
  (let [tab-state (r/atom :query)]
    (fn [val ctx props]
      (let [ctx (shadow-fiddle ctx)]
        [:div props
         [:h3 "Source: " (str @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/ident]))]
         [horizontal-tabs
          ; F U recom: Validation failed: Expected 'vector of tabs | atom'. Got '[:query :links :view :css :fiddle]'
          :tabs [{:id :query} {:id :links} {:id :view} {:id :css} {:id :fiddle}]
          :id-fn :id
          :label-fn (comp name :id)
          :model tab-state
          :on-change (r/partial reset! tab-state)]
         [(get tabs @tab-state) val ctx]]))))
