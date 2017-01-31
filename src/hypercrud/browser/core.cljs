(ns hypercrud.browser.core
  (:require [cats.monad.exception :as exception]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.system-links :as system-links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]
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
                                          (hc/hydrate (:peer param-ctx) query-value))))]
    (try
      (render-fn resultset link param-ctx)
      (catch :default e (pr-str e)))))


(defn request-for-link [link-dbid]
  (->EntityRequest link-dbid (->DbVal hc/*root-conn-id* nil)
                   '[* {:link/dbhole [* {:dbhole/value [*]}]
                        ; get all our forms for this link
                        :link/find-element
                        [* {:find-element/form
                            [* {:form/field
                                [*
                                 {:field/attribute [*
                                                    {:attribute/valueType [:db/id :db/ident]}
                                                    {:attribute/cardinality [:db/id :db/ident]}
                                                    {:attribute/unique [:db/id :db/ident]}]}
                                 ; we don't have to recurse for options-link-ctxs
                                 ; because we know these links don't have additional links
                                 {:field/options-link-ctx [* {:link-ctx/link
                                                              [* {:link/find-element
                                                                  [* {:find-element/form
                                                                      [* {:form/field
                                                                          [*
                                                                           {:field/attribute [*
                                                                                              {:attribute/valueType [:db/id :db/ident]}
                                                                                              {:attribute/cardinality [:db/id :db/ident]}
                                                                                              {:attribute/unique [:db/id :db/ident]}]}]}]}]}]}]}]}]}]
                        ; get links one layer deep; todo not sure if we need this
                        :link/link-ctx [* {:link-ctx/link [*]}]}
                     {:hypercrud/owner [*]}]))


(defn ui [{query-params :query-params :as params-map}
          {:keys [peer] :as param-ctx}]
  (let [system-link? (vector? (-> params-map :link-dbid :id))
        link (if system-link?
               (let [[_ _ system-link-name find-element-id] (-> params-map :link-dbid :id)
                     parent-link-dbid (:link-dbid params-map)]
                 (condp = system-link-name
                   :system-edit (system-links/system-edit-link find-element-id parent-link-dbid)
                   :system-create (system-links/system-create-link find-element-id parent-link-dbid)))
               (exception/extract (hc/hydrate peer (request-for-link (:link-dbid params-map)))))
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
      (let [resultset (->> (hc/hydrate peer (q-util/query-value q link params-map param-ctx))
                           (exception/extract))]
        ; you still want the param-ctx; just bypass the :link/bindings

        (condp = (get param-ctx :display-mode :dressed)
          :dressed (user-resultset resultset link (user-bindings link param-ctx))
          :undressed (auto-control/resultset resultset link (user-bindings link param-ctx))

          ; raw ignores user links and gets free system links
          :raw (let [link (system-links/overlay-system-links-tx link)
                     ; sub-queries (e.g. combo boxes) will get the old pulled-tree
                     ; Since we only changed link, this is only interesting for the hc-in-hc case
                     ]
                 (auto-control/resultset resultset link param-ctx)))))))


(declare request)


(defn field-requests [param-ctx field]
  (let [{:keys [:attribute/valueType :attribute/isComponent]} (:field/attribute field)
        is-ref (= (:db/ident valueType) :db.type/ref)]
    ; if we are a ref we ALWAYS need the query from the field options
    ; EXCEPT when we are component, in which case no options are rendered, just a form, handled below
    (if (and is-ref (not isComponent))
      (if-let [options-link-ctx (:field/options-link-ctx field)]
        (let [param-ctx (assoc param-ctx :debug (str "field-options:" (:db/id field)))]
          (request (links/build-url-params-map options-link-ctx param-ctx) param-ctx false))))))


(defn form-option-requests "get the form options recursively for all expanded forms"
  [form param-ctx]
  (mapcat #(field-requests param-ctx %) (:form/field form)))


(defn dependent-requests [{find-elements :link/find-element :as link} resultset param-ctx]
  (->> resultset
       (mapcat (fn [result]
                 (let [param-ctx (assoc param-ctx :result result)
                       option-requests (mapcat (fn [{form :find-element/form :as find-element}]
                                                 (form-option-requests form param-ctx))
                                               find-elements)
                       inline-requests (->> (:link/link-ctx link)
                                            (filter :link-ctx/render-inline?)
                                            (mapcat (fn [inline-link-ctx]
                                                      (let [param-ctx (assoc param-ctx :debug (str "inline-query:" (:db/id inline-link-ctx)))]
                                                        (request (links/build-url-params-map inline-link-ctx param-ctx) param-ctx true)))))]
                   (concat option-requests inline-requests))))))


(defn requests-for-link [{find-elements :link/find-element :as link} query-params
                         {:keys [peer] :as param-ctx} recurse?]
  (let [q (some-> link :link/query reader/read-string)
        params-map (merge query-params (q-util/build-dbhole-lookup link))
        param-ctx (assoc param-ctx :query-params query-params)]
    (if-not (links/holes-filled? (q-util/parse-holes q) params-map)
      (.log js/console (pr-str (->> (q-util/parse-holes q)
                                    (mapv (juxt identity #(get params-map %)))
                                    (into {}))))
      (let [request (q-util/query-value q link params-map param-ctx)]
        (concat
          (->> find-elements
               (mapv #(->DbVal (-> % :find-element/connection :db/id :id) nil)))
          [request]
          (if recurse?
            (if-let [resultset (exception/extract (hc/hydrate peer request) nil)]
              (dependent-requests link resultset param-ctx))))))))


(defn request [params-map param-ctx recurse?]
  (if (vector? (-> params-map :link-dbid :id))
    (let [[parent-link-id parent-link-conn-id system-link-name find-element-id] (-> params-map :link-dbid :id)
          parent-link-dbid (->DbId parent-link-id parent-link-conn-id)
          parent-link-request (request-for-link parent-link-dbid)]
      (concat
        [parent-link-request]
        (if-let [parent-link (-> (hc/hydrate (:peer param-ctx) parent-link-request)
                                 (exception/extract nil))]
          (let [find-element (->> (:link/find-element parent-link)
                                  (filter #(= find-element-id (-> % :db/id :id)))
                                  first)
                link (condp = system-link-name
                       :system-edit (system-links/system-edit-link find-element parent-link-dbid)
                       :system-create (system-links/system-create-link find-element parent-link-dbid))]
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
