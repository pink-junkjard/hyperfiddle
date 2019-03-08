(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.pprint]
    [contrib.data :refer [assoc-if]]
    [contrib.ednish]
    [contrib.rfc3986]
    [contrib.reactive :as r]
    [contrib.hfrecom :refer [anchor-tabs]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [anchor field value hyper-control link table]]

    ))


(defn with-fiddle-default [props fiddle-val attr]
  (if-some [f (get fiddle/fiddle-defaults attr)]
    (assoc props :default-value (f fiddle-val))
    props))

(defn schema-links [ctx]
  (->> (runtime/domain (:peer ctx)) ::ide-domain/user-dbname->ide keys sort
       (map (fn [user-dbname]
              (let [props {:route [(keyword "hyperfiddle.ide.schema" user-dbname)]
                           #_#_:target "_blank"}]
                ^{:key user-dbname}
                [anchor ctx props user-dbname])))
       (doall)))

(defn query-composite-stable [ctx-top val ctx-fiddle-type props]
  ; The divs are for styling
  [:div (select-keys props [:class])
   [hyper-control val ctx-fiddle-type (with-fiddle-default props val :fiddle/type)]
   [:div.pull-db
    (if (= val :entity)
      (let [ctx (context/focus ctx-top [:fiddle/pull-database])]
        [hyper-control (context/v ctx) ctx (with-fiddle-default {:class "pull-database"} val :fiddle/pull-database)]))]
   [:span.schema "schema: " (schema-links ctx-fiddle-type)]])

(let [link-fiddle (fn [val ctx props]
                    [:<>
                     [hyper-control val ctx props]
                     [link :hyperfiddle.ide/new-fiddle ctx]])
      empty-renderer (fn [val ctx props]
                       [:<>
                        (link #{:fiddle/links :hf/remove} ctx)])
      link-control (fn [val ctx props]
                     (let [props (assoc props :default-value (get (::record ctx) (context/a ctx)))]
                       (hyper-control val ctx props)))]
  (defn links-renderer [val ctx props]
    [:div
     (link #{:fiddle/links :hyperfiddle.ide/new-link} ctx)
     [table
      (fn [ctx]
        (let [ctx (assoc-if ctx ::record (if-not (:hypercrud.browser/head-sentinel ctx)
                                           ; todo shouldn't this be a meta-ctx? all these arguments look extremely suspect
                                           (-> (fiddle/auto-link+ @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas])
                                                                  (:qin @(:hypercrud.browser/qparsed ctx))
                                                                 (context/data ctx))
                                               ; just throw, unlikely we can ever get this far if there was an issue
                                               (either/branch (fn [e] (throw e)) identity))))]
          [(field [:link/path] ctx link-control)
           (field [:link/fiddle] ctx link-fiddle {:options :hyperfiddle.ide/fiddle-options
                                                  :option-label (r/comp pr-str :fiddle/ident)})
           (field [:link/class] ctx link-control)
           (field [:link/tx-fn] ctx link-control)
           (when (exists? js/show_formulas)
             (field [:link/formula] ctx link-control))
           (field [:db/id] ctx)]))
      ctx
      props]]))

(def tabs
  {:hf/query
   (fn [val ctx props]                                      ; val is the toplevel val
     [:<>
      (value [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
      (case (or (:fiddle/type val)
                ((:fiddle/type fiddle/fiddle-defaults)
                  val))
        :entity ^{:key "pull"} [value [:fiddle/pull] ctx hyper-control (with-fiddle-default props val :fiddle/pull)]
        :query ^{:key "query"} [value [:fiddle/query] ctx hyper-control (with-fiddle-default props val :fiddle/query)]
        :blank nil)])

   :hf/links
   (fn [val ctx props]
     (value [:fiddle/links] ctx links-renderer props))

   :hf/cljs
   (fn [val ctx props]
     (value [:fiddle/cljs-ns] ctx hyper-control (with-fiddle-default props val :fiddle/cljs-ns)))

   :hf/view
   (fn [val ctx props]
     (value [:fiddle/renderer] ctx hyper-control (with-fiddle-default props val :fiddle/renderer)))

   :hf/markdown
   (fn [val ctx props]
     (let [props (with-fiddle-default props val :fiddle/markdown)]
       (value [:fiddle/markdown] ctx hyper-control props)))

   :hf/css
   (fn [val ctx props]
     (let [props (with-fiddle-default props val :fiddle/css)]
       (value [:fiddle/css] ctx hyper-control props)))

   :hf/fiddle
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
                     (hypercrud.browser.context/validation-hints-here ctx)))}]])
   :hf/edn
   (fn [v ctx props]
     [contrib.ui/code {:value (contrib.pprint/pprint-str v 50) :read-only true}])})

(defn ^:export fiddle-src-renderer [_ ctx props]
  (let [tab-state (r/atom (if (contains? tabs (:initial-tab props)) (:initial-tab props) :hf/query))]
    (fn [_ ctx props]
      (let [ctx (hypercrud.browser.context/element ctx 0)
            val (hypercrud.browser.context/data ctx)]
        [:<>
         [:div.container-fluid (into {:key (str (:fiddle/ident val))} (select-keys props [:class]))
          [anchor-tabs
           :tabs (->> [:hf/query :hf/links :hf/markdown :hf/view :hf/cljs :hf/css :hf/fiddle :hf/edn]
                      (map (fn [k]
                             {:id k
                              ; Fragments mess with routing, they throw away the file segment on click
                              :href "" #_(->> (pr-str k)
                                         contrib.ednish/encode-ednish
                                         contrib.rfc3986/encode-rfc3986-pchar
                                         (str js/document.location.pathname "#"))})))
           :id-fn :id
           :label-fn (comp name :id)
           :model tab-state
           :on-change (r/partial reset! tab-state)]
          [(get tabs @tab-state) val ctx {}]]
         (when (exists? js/hyperfiddle_show_ide_stage)
           [hyperfiddle.ide/ide-stage ctx])]))))
