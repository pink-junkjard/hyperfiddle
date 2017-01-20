(ns hypercrud.browser.core
  (:require [cljs.core.match :refer-macros [match]]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->Entity]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]))


(defn pull-resultset [super-graph {find-elements :link/find-element :as link} create-new-find-elements resultset]
  (let [find-element-lookup (->> (mapv (juxt :find-element/name identity) find-elements)
                                 (into {}))]
    (->> (if (and (:link/single-result-as-entity? link) (= 0 (count resultset)))
           (let [local-result (->> find-elements
                                   (mapv (juxt :find-element/name #(get create-new-find-elements (:find-element/name %))))
                                   (into {}))]
             [local-result])
           resultset)
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
                                              params-map (links/build-params-map link-ctx param-ctx)
                                              query-value (let [q (some-> link :link/query reader/read-string)
                                                                params-map (merge (:query-params params-map)
                                                                                  (q-util/build-dbhole-lookup link))]
                                                            (q-util/query-value q link params-map param-ctx))]
                                          (pull-resultset (:super-graph param-ctx) link (:create-new-find-elements params-map)
                                                          (hc/select (:super-graph param-ctx) (hash query-value))))))]
    (try
      (render-fn resultset link param-ctx)
      (catch :default e (pr-str e)))))


(defn ui [{query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
          {:keys [super-graph] :as param-ctx}]
  (let [root-dbgraph (hc/get-dbgraph (:super-graph param-ctx) (->DbVal hc/*root-conn-id* nil))
        link (hc/entity root-dbgraph (:link-dbid params-map))
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
      (let [resultset (pull-resultset super-graph link create-new-find-elements
                                      (hc/select super-graph (hash (q-util/query-value q link params-map param-ctx))))]

        ; you still want the param-ctx; just bypass the :link/bindings

        (condp = (get param-ctx :display-mode :dressy)
          :dressy (user-resultset resultset link (user-bindings link param-ctx))
          :casual (auto-control/resultset resultset link (user-bindings link param-ctx))
          :naked (auto-control/resultset resultset link param-ctx))))))


(declare query)


(defn field-queries [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-link-ctx (:field/options-link-ctx field)]
        (let [param-ctx (assoc param-ctx :debug (str "field-options:" (:db/id field)))]
          (query (links/build-params-map options-link-ctx param-ctx) param-ctx false))))))


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
                                                     (query (links/build-params-map inline-link-ctx param-ctx) param-ctx true)))))]
                   (concat option-queries inline-queries))))
       (apply merge)))


(defn query [{query-params :query-params create-new-find-elements :create-new-find-elements :as params-map}
             {:keys [super-graph] :as param-ctx}
             recurse?]
  (let [root-dbval (->DbVal hc/*root-conn-id* nil)
        root-dbgraph (hc/get-dbgraph (:super-graph param-ctx) root-dbval)
        link-query ['[:find ?link :in $ ?link :where [?link]]
                    {"$" root-dbval "?link" (:link-dbid params-map)}
                    {"?link" [root-dbval '[* {:link/dbhole [* {:dbhole/value [*]}]
                                              ; get all our forms for this link
                                              :link/find-element [* {:find-element/form
                                                                     [* {:form/field
                                                                         ; we don't have to recurse for options-link-ctxs
                                                                         ; because we know these links don't have additional links
                                                                         [* {:field/options-link-ctx [* {:link-ctx/link [*]}]}]}]}]
                                              ; get links one layer deep; todo not sure if we need this
                                              :link/link-ctx [* {:link-ctx/link [*]}]}
                                           {:hypercrud/owner [*]}]]}]]
    (merge {(hash link-query) link-query}
           (if-let [{find-elements :link/find-element :as link} (some->> (-> (hc/select super-graph (hash link-query))
                                                                             first
                                                                             (get "?link"))
                                                                         (hc/entity root-dbgraph))]
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
                          (mapv #(schema-util/query-schema root-dbval %))
                          (apply merge))
                     {(hash result-query) result-query}
                     (if recurse?
                       (if-let [resultset (some->> (hc/select super-graph (hash result-query))
                                                   (pull-resultset super-graph link create-new-find-elements))]
                         (dependent-queries link resultset param-ctx)))))))))))


(defn replace-tempids-in-params-map [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        (update :query-params #(util/map-values replace-tempid %))
        (update :create-new-find-elements (fn [old-map]
                                            ; generate a new tempid for each existing tempid
                                            ; we don't want to replace here
                                            (util/map-values
                                              #(hc/*temp-id!* (:conn-id %))
                                              old-map))))))
