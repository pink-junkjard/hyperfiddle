(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [clojure.spec.alpha :as s]
    [clojure.pprint]
    [contrib.reactive :as r]
    [clojure.string :as string]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link table]]
    [re-com.tabs :refer [horizontal-tabs]]))


(defn with-fiddle-default [props fiddle-val attr]
  (if-some [f (get fiddle/fiddle-defaults attr)]
    (assoc props :default-value (f fiddle-val))
    props))

(defn schema-links [ctx]
  (->> (runtime/domain (:peer ctx))
       domain/databases
       keys
       (filter #(string/starts-with? % ide-domain/app-dbname-prefix))
       (map #(subs % 5))
       sort
       (map (fn [dbname]
              (let [props {:route [:hyperfiddle.ide/schema [dbname]]
                           #_#_:target "_blank"}]
                ^{:key dbname}
                [anchor ctx props dbname])))
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
                     [link :hyperfiddle.ide/new-fiddle ctx]])
      empty-renderer (fn [val ctx props]
                       (link #{:fiddle/links :hf/remove} ctx))
      link-control (fn [val ctx props]
                     (let [props (if-some [f (get fiddle/link-defaults (last (:hypercrud.browser/path ctx)))]
                                   (assoc props :default-value @(r/fmap f (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))
                                   props)]
                       (hyper-control val ctx props))
                     )]
  (defn links-renderer [val ctx props]
    [:div
     (link :hyperfiddle.ide/new-link ctx)
     [table
      (fn [ctx]
        [(field [:link/path] ctx link-control)
         (field [:link/fiddle] ctx link-fiddle {:options :hyperfiddle.ide/fiddle-options
                                                :option-label (r/comp pr-str :fiddle/ident first)})
         (field [:link/class] ctx link-control)
         (field [:link/tx-fn] ctx link-control)
         #_(field [:link/formula] ctx link-control)
         (field [] ctx empty-renderer)])
      ctx
      props]]))

(def tabs
  {:hf.src/query
   (fn [val ctx props]
     [:<>
      (field [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
      (case (or @(r/cursor (:hypercrud.browser/data ctx) [:fiddle/type]) ((:fiddle/type fiddle/fiddle-defaults) val))
        :entity ^{:key "pull"} [field [:fiddle/pull] ctx hyper-control (with-fiddle-default props val :fiddle/pull)]
        :query ^{:key "query"} [field [:fiddle/query] ctx hyper-control (with-fiddle-default props val :fiddle/query)]
        :blank nil)])

   :hf.src/links
   (fn [val ctx props]
     (field [:fiddle/links] ctx links-renderer props))

   :hf.src/ns
   (fn [val ctx props]
     (field [:fiddle/cljs-ns] ctx hyper-control (with-fiddle-default props val :fiddle/cljs-ns)))

   :hf.src/view
   (fn [val ctx props]
     (field [:fiddle/renderer] ctx hyper-control (with-fiddle-default props val :fiddle/renderer)))

   :hf.src/markdown
   (fn [val ctx props]
     (let [props (with-fiddle-default props val :fiddle/markdown)]
       (field [:fiddle/markdown] ctx hyper-control props)))

   :hf.src/css
   (fn [val ctx props]
     (let [props (with-fiddle-default props val :fiddle/css)]
       (field [:fiddle/css] ctx hyper-control props)))

   :hf.src/fiddle
   (fn [val ctx props]
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
                            [:div (link #{:hyperfiddle/ide :hf/remove} ctx "Remove fiddle" {:class "btn-outline-danger"})]))
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
                     (:hypercrud.browser/validation-hints ctx)))}]])})

(defn fiddle-src-renderer [val ctx props]
  (let [tab-state (r/atom (if (contains? tabs (:initial-tab props)) (:initial-tab props) :hf.src/query))]
    (fn [val ctx props]
      [:div (into {:key (str (:fiddle/ident val))} (select-keys props [:class]))
       ; Design constraint: one codemirror per tab, and it will expand to fill height.
       [horizontal-tabs
        ; F U recom: Validation failed: Expected 'vector of tabs | atom'. Got '[:query :links :view :css :fiddle]'
        :tabs (->> [:hf.src/query :hf.src/links :hf.src/markdown :hf.src/view :hf.src/ns :hf.src/css :hf.src/fiddle]
                   (map (partial hash-map :id)))
        :id-fn :id
        :label-fn (comp name :id)
        :model tab-state
        :on-change (r/partial reset! tab-state)]
       [(get tabs @tab-state) val ctx {}]])))
