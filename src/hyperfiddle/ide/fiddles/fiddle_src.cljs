(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.core :as cats]
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.ide.fiddles.fiddle-links.renderer :as links-fiddle]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.ide.system-fiddle :as system-fiddle]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link markdown]]
    [hyperfiddle.ui.controls :refer [label-with-docs]]
    [re-com.tabs :refer [horizontal-tabs]]
    [taoensso.timbre :as timbre]))


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

(defn links-composite-stable [val ctx props]
  (let [ctx (if (:embed-mode props)
              (links-fiddle/inject-topnav-links+ ctx)
              ctx)]
    [:div
     (if-not (:embed-mode props)           ; more work to be done to get this popover hydrating
       (link :hf/affix :link ctx "affix"))
     [links-fiddle/renderer val ctx props]]))

(defn query-composite-stable [ctx-top val ctx-fiddle-type props]
  ; The divs are for styling
  [:div
   [hyper-control val ctx-fiddle-type props]
   (if (= val :entity)
     (let [ctx (context/focus ctx-top [:fiddle/pull-database])]
       [:div [hyper-control @(:hypercrud.browser/data ctx) ctx {:class "pull-database"}]])
     [:div])
   [:span.schema "schema: " (schema-links ctx-fiddle-type)]])

; inline sys-link data when the entity is a system-fiddle
(letfn [(-shadow-fiddle [target-ident fiddle-val]
          (cond
            (system-fiddle/system-fiddle? target-ident) (->> (system-fiddle/hydrate-system-fiddle target-ident)
                                                             (cats/fmap #(fiddle/fiddle-defaults % nil))
                                                             (unwrap #(timbre/error %)))

            (nil? (:db/id fiddle-val)) fiddle-val
            :else (fiddle/fiddle-defaults fiddle-val nil)))]
  (defn shadow-fiddle [ctx]
    {:pre [(-> ctx :hypercrud.browser/data)]}
    (let [route @(:hypercrud.browser/route ctx)
          [_ [e]] route                                     ; [:hyperfiddle/topnav [#entity["$" [:fiddle/ident :hyperfiddle.system/remove]]]]
          [_ target-fiddle-ident] (:db/id e)]
      (update ctx :hypercrud.browser/data (partial r/fmap (r/partial -shadow-fiddle target-fiddle-ident))))))

(def tabs
  {:query (fn [val ctx props]
            (fragment
              (field [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
              (case @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/type])
                :entity (field [:fiddle/pull] ctx hyper-control props)
                :query (field [:fiddle/query] ctx hyper-control props)
                :blank nil)))
   :links (fn [val ctx props]
            ; Careful: pass through :embed-mode
            (field [:fiddle/links] ctx links-composite-stable props))
   :view (fn [val ctx props]
           (field [:fiddle/renderer] ctx hyper-control props))
   :markdown (fn [val ctx props]
               (field [:fiddle/markdown] ctx hyper-control props))
   :css (fn [val ctx props]
          (field [:fiddle/css] ctx hyper-control props))
   :fiddle (fn [val ctx props]
             (fragment
               (field [:fiddle/ident] ctx hyper-control (merge props {:read-only true}))
               (field [:fiddle/hydrate-result-as-fiddle] ctx hyper-control props)
               [:div.p "Additional attributes"]
               (->> @(r/fmap ::field/children (:hypercrud.browser/field ctx))
                    ; todo tighter reactivity
                    (map ::field/path-segment)
                    (remove #(= (namespace %) "fiddle"))
                    (remove #(= :db/id %))
                    (map (fn [segment]
                           ^{:key (str [segment])}
                           [field [segment] ctx nil]))
                    (doall))
               (field [:db/id] ctx (fn [val ctx props]
                                     [:div (link :hf/remove :fiddle ctx "Remove fiddle" {:class "btn-outline-danger"})]))))})

(defn fiddle-src-renderer [val ctx props]
  (let [tab-state (r/atom :query)]
    (fn [val ctx {:keys [:embed-mode] :as props}]
      (let [ctx (shadow-fiddle ctx)]
        [:div props
         #_(if-not embed-mode
           [:h3 "Source: " (str @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/ident]))])
         ; Design constraint: one codemirror per tab, and it will expand to fill height.
         [horizontal-tabs
          ; F U recom: Validation failed: Expected 'vector of tabs | atom'. Got '[:query :links :view :css :fiddle]'
          :tabs [{:id :query} {:id :links} {:id :markdown} {:id :view} {:id :css} {:id :fiddle}]
          :id-fn :id
          :label-fn (comp name :id)
          :model tab-state
          :on-change (r/partial reset! tab-state)]
         [(get tabs @tab-state) val ctx {:embed-mode embed-mode}]]))))
