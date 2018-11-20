(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.pprint]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.fiddle :as fiddle]
    #_[hyperfiddle.ide.hf-live :as hf-live]                 ;cycle
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link table]]
    [re-com.tabs :refer [horizontal-tabs]]
    [hypercrud.browser.base :as base]
    [hyperfiddle.fiddle :as fiddle]))


(defn with-fiddle-default [props fiddle-val attr]
  (if-some [f (get fiddle/fiddle-defaults attr)]
    (assoc props :default-value (f fiddle-val))
    props))

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

(defn query-composite-stable [ctx-top val ctx-fiddle-type props]
  ; The divs are for styling
  [:div
   [hyper-control val ctx-fiddle-type (with-fiddle-default props val :fiddle/type)]
   (if (= val :entity)
     (let [ctx (context/focus ctx-top [:fiddle/pull-database])]
       [:div [hyper-control @(:hypercrud.browser/data ctx) ctx (with-fiddle-default {:class "pull-database"} val :fiddle/pull-database)]])
     [:div])
   [:span.schema "schema: " (schema-links ctx-fiddle-type)]])

(let [link-fiddle (fn [val ctx props]
                    [:<>
                     [hyper-control val ctx props]
                     [link :hf/affix :fiddle ctx "affix"]])
      empty-renderer (fn [val ctx props]
                       (link :hf/remove :link ctx "remove"))
      target-ide-route (fn [ctx] (hyperfiddle.ide/ide-route (context/target-route ctx) ctx))
      inject-ide-links+ (fn [ctx]
                          (let [ctx (update ctx :hypercrud.browser/field #(r/fmap-> % (dissoc :hypercrud.browser.field/children)))]
                            ; Hacks because hf-live is not yet modeled in the fiddle-graph, we hardcode knowledge of the IDE fiddle-graph instead
                            (mlet [req (base/meta-request-for-fiddle
                                         (assoc ctx
                                           :hypercrud.browser/route (r/track target-ide-route ctx)
                                           :branch nil))
                                   src-fiddle (base/hydrate-fiddle (r/track identity req) ctx)]
                              (return
                                (update ctx :hypercrud.browser/fiddle #(r/fmap-> % (assoc :fiddle/links (:fiddle/links src-fiddle))))))))
      link-control (fn [val ctx props]
                     (let [props (if-some [f (get fiddle/link-defaults (last (:hypercrud.browser/path ctx)))]
                                   (assoc props :default-value @(r/fmap f (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))
                                   props)]
                       (hyper-control val ctx props))
                     )]
  (defn links-renderer [val ctx {:keys [:embed-mode] :as props}]
    (let [ctx (if embed-mode
                (either/branch
                  (inject-ide-links+ ctx)
                  (fn [e] (throw e))
                  identity)
                ctx)]
      [:div
       (link :hf/affix :link ctx "affix")
       [table
        (fn [ctx]
          [(field [:link/rel] ctx link-control)
           (field [:link/class] ctx link-control)
           (field [:link/fiddle] ctx link-fiddle {:options "fiddle-options"
                                                  :option-label (r/comp pr-str :fiddle/ident first)})
           (field [:link/path] ctx link-control)
           (field [:link/formula] ctx link-control)
           (field [:link/tx-fn] ctx link-control)
           (field [] ctx empty-renderer)])
        ctx
        (dissoc props :embed-mode)]])))

(def tabs
  {:hf.src/query
   (fn [val ctx props]
     (let [props (dissoc props :embed-mode)]
       [:<>
        (field [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
        (case (or @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/type]) ((:fiddle/type fiddle/fiddle-defaults) val))
          :entity ^{:key "pull"} [field [:fiddle/pull] ctx hyper-control (with-fiddle-default props val :fiddle/pull)]
          :query ^{:key "query"} [field [:fiddle/query] ctx hyper-control (with-fiddle-default props val :fiddle/query)]
          :blank nil)]))

   :hf.src/links
   (fn [val ctx props]
     ; Careful: pass through :embed-mode
     (field [:fiddle/links] ctx links-renderer props))

   :hf.src/view
   (fn [val ctx props]
     (let [props (-> (dissoc props :embed-mode))]
       [:<>
        (field [:fiddle/renderer] ctx hyper-control (with-fiddle-default props val :fiddle/renderer))
        (field [:fiddle/cljs-ns] ctx hyper-control (with-fiddle-default props val :fiddle/cljs-ns))]))

   :hf.src/markdown
   (fn [val ctx props]
     (let [props (-> (dissoc props :embed-mode)
                     (with-fiddle-default val :fiddle/markdown))]
       (field [:fiddle/markdown] ctx hyper-control props)))

   :hf.src/css
   (fn [val ctx props]
     (let [props (-> (dissoc props :embed-mode)
                     (with-fiddle-default val :fiddle/css))]
       (field [:fiddle/css] ctx hyper-control props)))

   :hf.src/fiddle
   (fn [val ctx props]
     (let [props (dissoc props :embed-mode)]
       [:<>
        (field [:fiddle/ident] ctx hyper-control (assoc props :disabled true))
        (field [:fiddle/uuid] ctx hyper-control (assoc props :disabled true))
        (field [:fiddle/hydrate-result-as-fiddle] ctx hyper-control props)
        [:div.p "Additional attributes"]
        (->> @(r/fmap->> (:hypercrud.browser/field ctx)
                         ::field/children
                         (map ::field/path-segment)
                         (remove (r/comp (r/partial = "fiddle") namespace))
                         (remove (r/partial = :db/id)))
             (map (fn [segment]
                    ^{:key (str [segment])}
                    [field [segment] ctx nil]))
             (doall))
        (field [:db/id] ctx (fn [val ctx props]
                              [:div (link :hf/remove :fiddle ctx "Remove fiddle" {:class "btn-outline-danger"})]))
        #_[:div.p "Spec debugging"]
        #_[:pre
           (with-out-str
             (clojure.pprint/print-table
               (->> (:hypercrud.browser/spec-explain ctx)
                    ::s/problems
                    (map #(update % :pred s/abbrev))
                    (map #(select-keys % [:in :pred])))))]
        #_[contrib.ui/code
           {:read-only true
            :value (with-out-str
                     (clojure.pprint/pprint
                       (:hypercrud.browser/validation-hints ctx)))}]]))})

(defn fiddle-src-renderer [val ctx props]
  (let [tab-state (r/atom (if (contains? tabs (:initial-tab props)) (:initial-tab props) :hf.src/query))]
    (fn [val ctx {:keys [:embed-mode] :as props}]
      [:div (select-keys props [:class])
       #_(if-not embed-mode
           [:h3 "Source: " (str @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/ident]))])
       ; Design constraint: one codemirror per tab, and it will expand to fill height.
       [horizontal-tabs
        ; F U recom: Validation failed: Expected 'vector of tabs | atom'. Got '[:query :links :view :css :fiddle]'
        :tabs [{:id :hf.src/query} {:id :hf.src/links} {:id :hf.src/markdown}
               {:id :hf.src/view} {:id :hf.src/css} {:id :hf.src/fiddle}]
        :id-fn :id
        :label-fn (comp name :id)
        :model tab-state
        :on-change (r/partial reset! tab-state)]
       [(get tabs @tab-state) val ctx {:embed-mode embed-mode}]])))
