(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [clojure.spec.alpha :as s]
    [clojure.pprint]
    [contrib.data :refer [assoc-if]]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field hyper-control link table]]
    [re-com.tabs :refer [horizontal-tabs]]))


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
                           #_#_:target "_blank"}]
                ^{:key $db}
                [anchor ctx props $db])))
       (doall)))

(defn query-composite-stable [ctx-top val ctx-fiddle-type props]
  ; The divs are for styling
  [:div
   [hyper-control val ctx-fiddle-type (with-fiddle-default props val :fiddle/type)]
   (if (= val :entity)
     (let [ctx (context/focus ctx-top [:fiddle/pull-database])]
       [:div [hyper-control (get @(:hypercrud.browser/eav ctx) 2)
              ctx (with-fiddle-default {:class "pull-database"} val :fiddle/pull-database)]])
     [:div])
   [:span.schema "schema: " (schema-links ctx-fiddle-type)]])

(let [link-fiddle (fn [val ctx props]
                    [:<>
                     [hyper-control val ctx props]
                     [link :hyperfiddle.ide/new-fiddle ctx]])
      empty-renderer (fn [val ctx props]
                       (link #{:fiddle/links :hf/remove} ctx))
      link-control (fn [val ctx props]
                     (let [props (assoc props :default-value (get (::record ctx) (context/a ctx)))]
                       (hyper-control val ctx props)))]
  (defn links-renderer [val ctx props]
    [:div
     (link :hyperfiddle.ide/new-link ctx)
     [table
      (fn [ctx]
        (let [ctx (assoc-if ctx ::record (if-not (:hypercrud.browser/head-sentinel ctx)
                                           (fiddle/auto-link @(:hypercrud.browser/schemas ctx)
                                                             (:qin @(:hypercrud.browser/qparsed ctx))
                                                             (context/data ctx))))]
          [(field [:link/path] ctx link-control)
           (field [:link/fiddle] ctx link-fiddle {:options :hyperfiddle.ide/fiddle-options
                                                  :option-label (r/comp pr-str :fiddle/ident)})
           (field [:link/class] ctx link-control)
           (field [:link/tx-fn] ctx link-control)
           #_(field [:link/formula] ctx link-control)
           (field [] ctx empty-renderer)]))                 ; why does [:db/id] crash here?
      ctx
      props]]))

(def tabs
  {:hf.src/query
   (fn [val ctx props]                                      ; val is the toplevel val
     [:<>
      (field [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
      (case (or (:fiddle/type val)
                ((:fiddle/type fiddle/fiddle-defaults)
                  val))
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
      (field [:fiddle/ident] ctx hyper-control)
      (field [:fiddle/uuid] ctx hyper-control)
      (field [:fiddle/hydrate-result-as-fiddle] ctx hyper-control)
      [:div.p "Additional attributes"]
      (doall
        (for [[k _] (hypercrud.browser.context/spread-attributes ctx)
              :when (and (not= :db/id k)
                         (not= "fiddle" (namespace k)))]
          (field [k] ctx)))
      #_(field [:fiddle/ident] ctx (fn [val ctx props]
                                     [:div (link #{:fiddle/ident :hf/remove} ctx "Remove fiddle" {:class "btn-outline-danger"})]))
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
                     (hypercrud.browser.context/validation-hints-here ctx)))}]])})

(defn fiddle-src-renderer [_ ctx props]
  (let [tab-state (r/atom (if (contains? tabs (:initial-tab props)) (:initial-tab props) :hf.src/query))]
    (fn [_ ctx props]
      (let [ctx (hypercrud.browser.context/element ctx 0)
            val (hypercrud.browser.context/data ctx)]
        [:div (into {:key (str (:fiddle/ident val))} (select-keys props [:class]))
         [horizontal-tabs
          :tabs (->> [:hf.src/query :hf.src/links :hf.src/markdown :hf.src/view :hf.src/ns :hf.src/css :hf.src/fiddle]
                     (map (partial hash-map :id)))
          :id-fn :id
          :label-fn (comp name :id)
          :model tab-state
          :on-change (r/partial reset! tab-state)]
         [(get tabs @tab-state) val ctx {}]]))))
