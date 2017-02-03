(ns hypercrud.browser.core
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception :refer-macros [try-on]]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.system-links :as system-links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]))


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


(defn request-for-link [link-dbid]
  (let [inner-form-pull-exp ['* {:form/field
                                 ['*
                                  {:field/attribute ['*
                                                     {:attribute/valueType [:db/id :db/ident]}
                                                     {:attribute/cardinality [:db/id :db/ident]}
                                                     {:attribute/unique [:db/id :db/ident]}]}]}]
        form-pull-exp ['* {:form/field
                           ['*
                            {:field/attribute ['*
                                               {:attribute/valueType [:db/id :db/ident]}
                                               {:attribute/cardinality [:db/id :db/ident]}
                                               {:attribute/unique [:db/id :db/ident]}]}
                            ; we don't have to recurse for options-anchors
                            ; because we know these links don't have additional links
                            {:field/options-anchor ['* {:anchor/link
                                                        ['* {:link/request ['*
                                                                            {:link-entity/form inner-form-pull-exp}
                                                                            {:link-query/find-element ['* {:find-element/form inner-form-pull-exp}]}]}]}]}]}]]
    (->EntityRequest link-dbid (->DbVal hc/*root-conn-id* nil)
                     ['* {:link/request ['*
                                         :link-entity/connection
                                         {:link-entity/form form-pull-exp}
                                         :link-query/value
                                         :link-query/single-result-as-entity?
                                         {:link-query/dbhole ['* {:dbhole/value ['*]}]}
                                         ; get all our forms for this link
                                         {:link-query/find-element ['* {:find-element/form form-pull-exp}]}]
                          ; get links one layer deep; todo not sure if we need this
                          :link/anchor ['* {:anchor/link ['*]}]}
                      {:hypercrud/owner ['*]}])))


(defn user-resultset [resultset link param-ctx]
  (let [render-fn (if (empty? (:link/renderer link))
                    auto-control/resultset
                    (let [{f :value error :error} (eval (:link/renderer link))]
                      (if error
                        (fn [resultset link param-ctx]
                          [:pre (pprint/pprint error)])
                        f)))
        anchors (->> (:link/anchor link)
                     (mapv (juxt #(-> % :anchor/ident) identity))
                     (into {}))
        inline-resultset (fn [ident param-ctx]
                           (let [anchor (get anchors ident)
                                 params-map (links/build-url-params-map anchor param-ctx)
                                 query-params (:query-params params-map)]
                             (mlet [link (hc/hydrate (:peer param-ctx) (request-for-link (-> anchor :anchor/link :db/id)))
                                    query-value (try-on
                                                  (condp = (links/link-type link)
                                                    :link-query (let [q (some-> link :link/request :link-query/value reader/read-string)
                                                                      params-map (merge query-params
                                                                                        (q-util/build-dbhole-lookup (:link/request link)))]
                                                                  (q-util/query-value q (:link/request link) params-map param-ctx))

                                                    :link-entity (q-util/->entityRequest (:link/request link) query-params)
                                                    (js/Error. "Missing link request")))
                                    resultset (hc/hydrate (:peer param-ctx) query-value)]
                                   (cats/return resultset))))
        param-ctx (assoc param-ctx
                    :link-fn (fn [ident label param-ctx]
                               (let [anchor (get anchors ident)
                                     props (links/build-link-props anchor param-ctx)]
                                 [(:navigate-cmp param-ctx) props label param-ctx]))
                    :inline-resultset inline-resultset)]
    (try
      (render-fn resultset link param-ctx)
      (catch :default e (pr-str e)))))


(defn ui [{query-params :query-params :as params-map}
          {:keys [peer] :as param-ctx}]
  (let [param-ctx (assoc param-ctx :query-params query-params)
        dom-or-e (mlet [link (if (vector? (-> params-map :link-dbid :id))
                               (let [system-link-dbid (-> params-map :link-dbid :id)]
                                 (->> (system-links/request-for-system-link system-link-dbid)
                                      (hc/hydrate peer)
                                      (cats/fmap #(system-links/generate-system-link system-link-dbid %))))
                               (hc/hydrate peer (request-for-link (:link-dbid params-map))))
                        request (try-on
                                  (condp = (links/link-type link)
                                    :link-query (let [link-query (:link/request link)
                                                      q (some-> link-query :link-query/value reader/read-string)
                                                      params-map (merge query-params (q-util/build-dbhole-lookup link-query))]
                                                  (q-util/query-value q link-query params-map param-ctx))
                                    :link-entity (q-util/->entityRequest (:link/request link) (:query-params params-map))
                                    (js/Error. "Missing link request")))
                        resultset (hc/hydrate peer request)]
                       (cats/return (condp = (get param-ctx :display-mode :dressed)
                                      :dressed (user-resultset resultset link (user-bindings link param-ctx))
                                      :undressed (auto-control/resultset resultset link (user-bindings link param-ctx))

                                      ; raw ignores user links and gets free system links
                                      :raw (let [link (system-links/overlay-system-links-tx link) ;todo don't overlay system links on system links
                                                 ; sub-queries (e.g. combo boxes) will get the old pulled-tree
                                                 ; Since we only changed link, this is only interesting for the hc-in-hc case
                                                 ]
                                             (auto-control/resultset resultset link param-ctx)))))]
    (if (exception/failure? dom-or-e)
      [:div
       [:span (-> dom-or-e .-e .-msg)]
       [:pre (-> dom-or-e .-e .-stack)]]
      (.-v dom-or-e))))


(declare request)


(defn field-requests [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-anchor (:field/options-anchor field)]
        (let [params-map (links/build-url-params-map options-anchor param-ctx)
              param-ctx (-> param-ctx
                            (assoc :debug (str "field-options:" (:db/id field)))
                            (dissoc :entity :request))]
          (request params-map param-ctx false))))))


(defn form-option-requests "get the form options recursively for all expanded forms"
  [form param-ctx]
  (mapcat #(field-requests param-ctx %) (:form/field form)))


(defn dependent-requests [anchors forms param-ctx]
  (let [option-requests (mapcat #(form-option-requests % param-ctx) forms)
        inline-requests (->> anchors
                             (filter :anchor/render-inline?)
                             (mapcat (fn [inline-anchor]
                                       (let [params-map (links/build-url-params-map inline-anchor param-ctx)
                                             param-ctx (-> param-ctx
                                                           (assoc :debug (str "inline-query:" (:db/id inline-anchor)))
                                                           (dissoc :entity :request))]
                                         (request params-map param-ctx true)))))]
    (concat option-requests inline-requests)))


(defn requests-for-link-query [link query-params {:keys [peer] :as param-ctx} recurse?]
  (let [link-query (:link/request link)
        q (some-> link-query :link-query/value reader/read-string)
        params-map (merge query-params (q-util/build-dbhole-lookup link-query))
        param-ctx (assoc param-ctx :query-params query-params)]
    (let [request (q-util/query-value q link-query params-map param-ctx)]
      (concat
        (->> (:link-query/find-element link-query)
             (mapv #(->DbVal (-> % :find-element/connection :db/id :id) nil)))
        [request]
        (if recurse?
          (if-let [resultset (exception/extract (hc/hydrate peer request) nil)]
            (->> resultset
                 (mapcat (fn [result]
                           (let [forms (mapv :find-element/form (:link-query/find-element link-query))
                                 param-ctx (assoc param-ctx :result result)]
                             (dependent-requests (:link/anchor link) forms param-ctx)))))))))))


(defn requests-for-link-entity [link query-params {:keys [peer] :as param-ctx} recurse?]
  (let [request (q-util/->entityRequest (:link/request link) query-params)]
    (concat
      [(->DbVal (get-in link [:link/request :link-entity/connection :db/id :id]) nil)
       request]
      (if recurse?
        (if-let [response (exception/extract (hc/hydrate peer request) nil)]
          (let [form (get-in link [:link/request :link-entity/form])]
            (if (instance? types/DbId (:dbid-s request))
              (let [param-ctx (assoc param-ctx :entity response)]
                (dependent-requests (:link/anchor link) [form] param-ctx))
              (->> response
                   (mapcat (fn [entity]
                             (let [param-ctx (assoc param-ctx :entity entity)]
                               (dependent-requests (:link/anchor link) [form] param-ctx))))))))))))


(defn requests-for-link [link query-params param-ctx recurse?]
  (let [param-ctx (assoc param-ctx :query-params query-params)]
    (condp = (links/link-type link)
      :link-query (requests-for-link-query link query-params param-ctx recurse?)
      :link-entity (requests-for-link-entity link query-params param-ctx recurse?)
      nil)))


(defn request [params-map param-ctx recurse?]
  (if (vector? (-> params-map :link-dbid :id))
    (let [system-link-dbid (-> params-map :link-dbid :id)
          sytem-link-request (system-links/request-for-system-link system-link-dbid)]
      (concat
        [sytem-link-request]
        (if-let [system-link-deps (-> (hc/hydrate (:peer param-ctx) sytem-link-request)
                                      (exception/extract nil))]
          (let [link (system-links/generate-system-link system-link-dbid system-link-deps)]
            (requests-for-link link (:query-params params-map) param-ctx recurse?)))))
    (let [link-request (request-for-link (:link-dbid params-map))]
      (concat [link-request]
              (if-let [link (-> (hc/hydrate (:peer param-ctx) link-request)
                                (exception/extract nil))]
                (requests-for-link link (:query-params params-map) param-ctx recurse?))))))


(defn replace-tempids-in-params-map [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        (update :query-params #(util/map-values replace-tempid %)))))
