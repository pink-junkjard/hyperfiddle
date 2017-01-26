(ns hypercrud.browser.core
  (:require [cats.core :as cats]
            [cats.monad.exception :as exception]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.system-links :as system-links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]))


(defn pull-resultset [super-graph {find-elements :link/find-element :as link} resultset]
  (let [find-element-lookup (->> (mapv (juxt :find-element/name identity) find-elements)
                                 (into {}))]
    (->> resultset
         (mapv (fn [result]
                 (->> result
                      (mapv (fn [[find-element-symbol entity-dbid]]
                              (let [connection (get-in find-element-lookup [find-element-symbol :find-element/connection :db/id :id])
                                    dbval (->DbVal connection nil)]
                                [find-element-symbol (hc/entity (hc/get-dbgraph super-graph dbval) entity-dbid)])))
                      (into {})))))))


(defn user-bindings [link param-ctx]
  (let [bindings-fn (if (empty? (:link/bindings link))
                      identity
                      (let [{f :value error :error} (eval (:link/bindings link))]
                        (if error
                          (fn [_] (throw error))
                          f)))]
    (try
      (bindings-fn param-ctx)
      (catch :default error
        (js/alert (str "error in user-bindings:\n" (pr-str error)))
        param-ctx))))


(defn user-resultset [resultset link param-ctx]
  (let [render-fn (if (empty? (:link/renderer link))
                    auto-control/resultset
                    (let [{f :value error :error} (eval (:link/renderer link))]
                      (if error
                        (fn [resultset link param-ctx]
                          [:pre (pprint/pprint error)])
                        f)))
        link-ctxs (->> (:link/link-ctx link)
                       (mapv (juxt #(-> % :link-ctx/ident) identity))
                       (into {}))
        param-ctx (assoc param-ctx
                    :link-fn (fn [ident label param-ctx]
                               (let [link-ctx (get link-ctxs ident)
                                     props (links/build-link-props link-ctx param-ctx)]
                                 [(:navigate-cmp param-ctx) props label param-ctx]))
                    :inline-resultset (fn [ident param-ctx]
                                        (let [link-ctx (get link-ctxs ident)
                                              link (:link-ctx/link link-ctx)
                                              params-map (links/build-url-params-map link-ctx param-ctx)
                                              query-value (let [q (some-> link :link/query reader/read-string)
                                                                params-map (merge (:query-params params-map)
                                                                                  (q-util/build-dbhole-lookup link))]
                                                            (q-util/query-value q link params-map param-ctx))]
                                          (->> (hc/select (:super-graph param-ctx) (hash query-value))
                                               (cats/fmap #(pull-resultset (:super-graph param-ctx) link %))))))]
    (try
      (render-fn resultset link param-ctx)
      (catch :default e (pr-str e)))))


(defn with-parent-link [params-map param-ctx fn]
  (let [[parent-link-id parent-link-conn-id] (-> params-map :link-dbid :id)
        parent-link (hc/entity (hc/get-dbgraph (:super-graph param-ctx) (->DbVal hc/*root-conn-id* nil))
                               (->DbId parent-link-id parent-link-conn-id))]
    (fn parent-link)))


(defn ui [{query-params :query-params :as params-map}
          {:keys [super-graph] :as param-ctx}]
  (let [system-link? (vector? (-> params-map :link-dbid :id))
        param-ctx (if system-link?
                    (let [[_ _ system-link-name find-element-id] (-> params-map :link-dbid :id)]
                      (with-parent-link params-map param-ctx
                                        (fn [parent-link]
                                          (let [find-element (->> (:link/find-element parent-link)
                                                                  (filter #(= find-element-id (-> % :db/id :id)))
                                                                  first)
                                                tx (condp = system-link-name
                                                     :system-edit (system-links/manufacture-system-edit-tx find-element (:db/id parent-link))
                                                     :system-create (system-links/manufacture-system-create-tx find-element (:db/id parent-link)))]
                                            (update param-ctx :super-graph #(hc/with % tx))))))
                    param-ctx)
        root-dbgraph (hc/get-dbgraph (:super-graph param-ctx) (->DbVal hc/*root-conn-id* nil))
        link-dbid (if system-link?
                    (let [[_ _ system-link-name find-element-id] (-> params-map :link-dbid :id)]
                      (with-parent-link params-map param-ctx (fn [parent-link]
                                                               (condp = system-link-name
                                                                 :system-edit (system-links/system-edit-link-dbid find-element-id (:db/id parent-link))
                                                                 :system-create (system-links/system-create-link-dbid find-element-id (:db/id parent-link))))))
                    (:link-dbid params-map))
        link (hc/entity root-dbgraph link-dbid)
        q (some-> link :link/query reader/read-string)
        params-map (merge query-params (q-util/build-dbhole-lookup link))
        param-ctx (assoc param-ctx :query-params query-params)
        query-hole-names (q-util/parse-holes q)]
    (if-not (links/holes-filled? query-hole-names params-map) ;todo what if we have a user hole?
      [:div
       [:div "Unfilled query holes"]
       [:pre (doall (with-out-str
                      (binding [pprint/*print-miser-width* 1] ; not working
                        (pprint/pprint (->> query-hole-names
                                            (mapv (juxt identity #(get params-map %)))
                                            (into {}))))))]]
      (let [resultset (->> (hc/select super-graph (hash (q-util/query-value q link params-map param-ctx)))
                           (exception/extract)
                           (pull-resultset super-graph link))]
        ; you still want the param-ctx; just bypass the :link/bindings

        (condp = (get param-ctx :display-mode :dressed)
          :dressed (user-resultset resultset link (user-bindings link param-ctx))
          :undressed (auto-control/resultset resultset link (user-bindings link param-ctx))

          ; raw ignores user links and gets free system links
          :raw (let [overlay-tx (system-links/overlay-system-links-tx link)
                     link' (hc/entity (hc/with' root-dbgraph overlay-tx) (:db/id link))
                     ; sub-queries (e.g. combo boxes) will get the old supergraph
                     ; Since we only changed root graph, this is only interesting for the hc-in-hc case
                     ]
                 (auto-control/resultset resultset link' param-ctx)))))))


(declare query)


(defn field-queries [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-link-ctx (:field/options-link-ctx field)]
        (let [param-ctx (assoc param-ctx :debug (str "field-options:" (:db/id field)))]
          (query (links/build-url-params-map options-link-ctx param-ctx) param-ctx false))))))


(defn form-option-queries "get the form options recursively for all expanded forms"
  [form param-ctx]
  (apply merge
         (mapv #(field-queries param-ctx %) (:form/field form))))


(defn dependent-queries [{find-elements :link/find-element :as link} resultset param-ctx]
  (->> resultset
       (mapcat (fn [result]
                 (let [param-ctx (assoc param-ctx :result result)
                       option-queries (mapv (fn [{form :find-element/form :as find-element}]
                                              (form-option-queries form param-ctx))
                                            find-elements)
                       inline-queries (->> (:link/link-ctx link)
                                           (filter :link-ctx/render-inline?)
                                           (mapv (fn [inline-link-ctx]
                                                   (let [param-ctx (assoc param-ctx :debug (str "inline-query:" (.-dbid inline-link-ctx)))]
                                                     (query (links/build-url-params-map inline-link-ctx param-ctx) param-ctx true)))))]
                   (concat option-queries inline-queries))))
       (apply merge)))


(defn q-for-link [link-dbid]
  ['[:find ?link :in $ ?link :where [?link]]
   {"$" (->DbVal hc/*root-conn-id* nil) "?link" link-dbid}
   {"?link" [(->DbVal hc/*root-conn-id* nil) '[* {:link/dbhole [* {:dbhole/value [*]}]
                                                  ; get all our forms for this link
                                                  :link/find-element [* {:find-element/form
                                                                         [* {:form/field
                                                                             ; we don't have to recurse for options-link-ctxs
                                                                             ; because we know these links don't have additional links
                                                                             [* {:field/options-link-ctx [* {:link-ctx/link [*]}]}]}]}]
                                                  ; get links one layer deep; todo not sure if we need this
                                                  :link/link-ctx [* {:link-ctx/link [*]}]}
                                               {:hypercrud/owner [*]}]]}])


(defn query-for-link [{find-elements :link/find-element :as link} query-params
                      {:keys [super-graph] :as param-ctx} recurse?]
  (let [q (some-> link :link/query reader/read-string)
        params-map (merge query-params (q-util/build-dbhole-lookup link))
        param-ctx (assoc param-ctx :query-params query-params)]
    (if-not (links/holes-filled? (q-util/parse-holes q) params-map)
      (.log js/console (pr-str (->> (q-util/parse-holes q)
                                    (mapv (juxt identity #(get params-map %)))
                                    (into {}))))
      (let [result-query (q-util/query-value q link params-map param-ctx)]
        (merge
          (->> find-elements
               (mapv #(->DbVal (-> % :find-element/connection :db/id :id) nil))
               (mapv #(schema-util/query-schema (->DbVal hc/*root-conn-id* nil) %))
               (apply merge))
          {(hash result-query) result-query}
          (if recurse?
            (if-let [resultset (some->> (exception/extract (hc/select super-graph (hash result-query)) nil)
                                        (pull-resultset super-graph link))]
              (dependent-queries link resultset param-ctx))))))))


(defn query [params-map param-ctx recurse?]
  (if (vector? (-> params-map :link-dbid :id))
    (let [[parent-link-id parent-link-conn-id system-link-name find-element-id] (-> params-map :link-dbid :id)
          parent-link-dbid (->DbId parent-link-id parent-link-conn-id)
          root-dbval (->DbVal hc/*root-conn-id* nil)
          root-dbgraph (hc/get-dbgraph (:super-graph param-ctx) root-dbval)
          parent-link-query (q-for-link parent-link-dbid)]
      (merge {(hash parent-link-query) parent-link-query}
             (if-let [parent-link (some->> (-> (hc/select (:super-graph param-ctx) (hash parent-link-query))
                                               (exception/extract nil)
                                               first
                                               (get "?link"))
                                           (hc/entity root-dbgraph))]
               (let [find-element (->> (:link/find-element parent-link)
                                       (filter #(= find-element-id (-> % :db/id :id)))
                                       first)
                     link (let [tx (condp = system-link-name
                                     :system-edit (system-links/manufacture-system-edit-tx find-element parent-link-dbid)
                                     :system-create (system-links/manufacture-system-create-tx find-element parent-link-dbid))
                                link-dbid (condp = system-link-name
                                            :system-edit (system-links/system-edit-link-dbid find-element-id parent-link-dbid)
                                            :system-create (system-links/system-create-link-dbid find-element-id parent-link-dbid))]
                            (-> (hc/with' root-dbgraph tx)
                                (hc/entity link-dbid)))]
                 (query-for-link link (:query-params params-map) param-ctx recurse?)))))
    (let [root-dbval (->DbVal hc/*root-conn-id* nil)
          root-dbgraph (hc/get-dbgraph (:super-graph param-ctx) root-dbval)
          link-query (q-for-link (:link-dbid params-map))]
      (merge {(hash link-query) link-query}
             (if-let [link (some->> (-> (hc/select (:super-graph param-ctx) (hash link-query))
                                        (exception/extract nil)
                                        first
                                        (get "?link"))
                                    (hc/entity root-dbgraph))]
               (query-for-link link (:query-params params-map) param-ctx recurse?))))))


(defn replace-tempids-in-params-map [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        (update :query-params #(util/map-values replace-tempid %)))))
