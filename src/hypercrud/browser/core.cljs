(ns hypercrud.browser.core
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception :refer-macros [try-on]]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader]
            [hypercrud.browser.links :as links]
            [hypercrud.browser.system-links :as system-links]
            [hypercrud.client.core :as hc]
            [hypercrud.compile.eval :refer [eval-str]]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.ui.renderer :as renderer]))


(defn user-bindings [link param-ctx]
  (let [bindings-fn (if (empty? (:link/bindings link))
                      identity
                      (let [{f :value error :error} (eval-str (:link/bindings link))]
                        (if error
                          (fn [_] (throw error))
                          f)))]
    (try
      (bindings-fn param-ctx)
      (catch :default error
        (.warn js/console (str "error in user-bindings:\n" (pr-str error)))
        param-ctx))))


(defn request-for-link [link-dbid]
  (let [inner-form-pull-exp ['*
                             {:hypercrud/owner ['*]
                              :form/field
                              ['*
                               {:field/attribute ['*
                                                  {:attribute/valueType [:db/id :db/ident]
                                                   :attribute/cardinality [:db/id :db/ident]
                                                   :attribute/unique [:db/id :db/ident]}]}]}]
        form-pull-exp ['*
                       {:hypercrud/owner ['*]
                        :form/field
                        ['*
                         {:field/attribute ['*
                                            {:attribute/valueType [:db/id :db/ident]
                                             :attribute/cardinality [:db/id :db/ident]
                                             :attribute/unique [:db/id :db/ident]}]
                          ; we don't have to recurse for options-anchors
                          ; because we know these links don't have additional links
                          :field/options-anchor ['*
                                                 {:anchor/link
                                                  ['*
                                                   {:hypercrud/owner ['*]
                                                    :link/request ['*
                                                                   {:link-entity/form inner-form-pull-exp
                                                                    :link-query/find-element ['*
                                                                                              {:find-element/form inner-form-pull-exp}]}]}]}]}]}]]
    (->EntityRequest link-dbid (->DbVal hc/*root-conn-id* nil)
                     ['*
                      {:link/request ['*
                                      :link-query/value
                                      :link-query/single-result-as-entity?
                                      {:link-entity/connection [:db/id :database/ident]
                                       :link-entity/form form-pull-exp
                                       :link-query/dbhole ['* {:dbhole/value ['*]}]
                                       ; get all our forms for this link
                                       :link-query/find-element ['* {:find-element/form form-pull-exp
                                                                     :find-element/connection [:db/id :database/ident]}]}]
                       :link/anchor ['*
                                     {:anchor/link ['*      ; hydrate the whole link for validating the anchor by query params
                                                    {:hypercrud/owner ['*]}] ; need the link's owner to render the href to it
                                      :anchor/find-element [:db/id :find-element/name]}]
                       :hypercrud/owner ['*]}])))


(defn user-result [link]
  (let [render-fn (if (empty? (:link/renderer link))
                    auto-control/result
                    (let [{f :value error :error} (eval-str (:link/renderer link))]
                      (if error
                        (fn [type result colspec anchor-index param-ctx]
                          [:pre (pprint/pprint error)])
                        f)))]
    (fn [result colspec anchors param-ctx]
      (let [anchor-index (->> anchors
                              (mapv (juxt #(-> % :anchor/ident) identity))
                              (into {}))
            inline-result (fn [ident param-ctx]
                            (let [anchor (get anchor-index ident)
                                  params-map (links/build-url-params-map anchor param-ctx)
                                  query-params (:query-params params-map)]
                              (mlet [link (hc/hydrate (:peer param-ctx) (request-for-link (-> anchor :anchor/link :db/id)))
                                     request (try-on
                                               (case (links/link-type link)
                                                 :link-query (let [q (some-> link :link/request :link-query/value reader/read-string)
                                                                   params-map (merge query-params
                                                                                     (q-util/build-dbhole-lookup (:link/request link)))]
                                                               (q-util/query-value q (:link/request link) params-map param-ctx))

                                                 :link-entity (q-util/->entityRequest (:link/request link) query-params)
                                                 nil))
                                     result (if request
                                              (hc/hydrate (:peer param-ctx) request)
                                              (exception/success nil))]
                                    (cats/return result))))
            param-ctx (assoc param-ctx
                        :link-fn (fn [ident label param-ctx]
                                   (let [anchor (get anchor-index ident)
                                         props (-> (links/build-link-props anchor param-ctx)
                                                   #_(dissoc :style) #_ "custom renderers don't want colored links")]
                                     [(:navigate-cmp param-ctx) props label param-ctx]))
                        ;:inline-result inline-result
                        )]
        ; result is relation or set of relations
        [renderer/safe-user-renderer render-fn result colspec anchors param-ctx]))))

(defn merge-anchors [sys-anchors link-anchors]
  ; Merge the link-anchors into the sys-anchors such that matching anchors properly override.
  ; anchor uniqueness is determined by [repeat entity attr ident]. Nil ident means
  ; don't match anything. For example [nil nil nil nil] can just mean i have a lot
  ; of top level links that i didn't bother to name yet.
  (let [f (fn [e]
            [(or (-> e :anchor/repeating?) false)           ; nil matches false
             (-> e :anchor/find-element :db/id)
             (-> e :anchor/attribute :db/id)
             (or (-> e :anchor/ident) (:db/id e)) #_"if the link isn't named, it's unique"])
        collated (merge-with concat (group-by f sys-anchors) (group-by f link-anchors))
        merged (map #(apply merge %) (vals collated)) #_(apply map merge (vals collated))]
    merged))

(defn hydrate-link [peer link-dbid]
  (if (system-links/system-link? link-dbid)
    (let [system-link-id (-> link-dbid :id)]
      (->> (system-links/request-for-system-link system-link-id)
           (hc/hydrate peer)
           (cats/fmap #(system-links/generate-system-link system-link-id %))))
    (hc/hydrate peer (request-for-link link-dbid))))

(defn ui [{query-params :query-params :as params-map} param-ctx]
  (let [dom-or-e
        (mlet [link (hydrate-link (:peer param-ctx) (:link-dbid params-map))
               request (try-on
                         (case (links/link-type link)
                           :link-query (let [link-query (:link/request link)
                                             q (some-> link-query :link-query/value reader/read-string)
                                             params-map (merge query-params (q-util/build-dbhole-lookup link-query))]
                                         (q-util/query-value q link-query params-map param-ctx))
                           :link-entity (q-util/->entityRequest (:link/request link) (:query-params params-map))
                           nil))
               result (if request (hc/hydrate (:peer param-ctx) request) (exception/success nil))
               ; schema is allowed to be nil if the link only has anchors and no data dependencies
               schema (exception/try-or-else (hc/hydrate (:peer param-ctx) (schema-util/schema-request nil)) nil)]
              (cats/return
                (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                      param-ctx (assoc param-ctx            ; provide defaults before user-bindings run. TODO query side
                                  :query-params query-params
                                  :schema indexed-schema
                                  :read-only (or (:read-only param-ctx) (constantly false)))

                      ; ereq doesn't have a fe yet; wrap with a fe.
                      ; Doesn't make sense to do on server since this is going to optimize away anyway.
                      result (if (-> link :link/request :link-entity/connection)
                               (if (map? result)            ; But the ereq might return a vec for cardinality many
                                 {"entity" result}
                                 (mapv (fn [relation] {"entity" relation}) result))
                               result)
                      result (if (-> link :link/request :link-query/single-result-as-entity?) (first result) result) ; unwrap query into entity
                      colspec (form-util/determine-colspec result link param-ctx)
                      system-anchors (if-not (system-links/system-link? (-> params-map :link-dbid))
                                       (system-links/system-anchors link result param-ctx))]
                  (case (get param-ctx :display-mode :dressed)
                    :dressed ((user-result link) result colspec (merge-anchors system-anchors (:link/anchor link)) (user-bindings link param-ctx))
                    :undressed (auto-control/result result colspec (merge-anchors system-anchors (:link/anchor link)) (user-bindings link param-ctx))
                    :raw (auto-control/result result colspec system-anchors param-ctx)))))]
    (if (exception/failure? dom-or-e)
      (-> dom-or-e .-e .-data)
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
                            (update :debug #(str % ">field-options[" (:db/id field) "]"))
                            (dissoc :entity :request))]
          (request params-map param-ctx false))))))


(defn form-option-requests "get the form options recursively for all expanded forms"
  [form param-ctx]
  (mapcat #(field-requests param-ctx %) (:form/field form)))


(defn dependent-requests [resultset find-elements anchors param-ctx]
  (let [anchors (filter :anchor/render-inline? anchors)     ; at this point we only care about inline anchors
        repeating-anchors-lookup (->> anchors
                                      (filter :anchor/repeating?)
                                      (group-by (fn [anchor]
                                                  (if-let [find-element (:anchor/find-element anchor)]
                                                    (:find-element/name find-element)
                                                    (if (:anchor/attribute anchor)
                                                      ; entity links can have fields but not find-elements specified
                                                      "entity"
                                                      nil)))))
        recurse-request (fn [anchor param-ctx]
                          (let [params-map (links/build-url-params-map anchor param-ctx)
                                param-ctx (-> param-ctx
                                              (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                              (dissoc :entity :request))]
                            (request params-map param-ctx true)))]
    (concat
      ; non-repeating inline-links
      (->> anchors
           (remove :anchor/repeating?)
           (mapcat #(recurse-request % param-ctx)))

      ; repeating inline-links (require :result and :entity in param-ctx)
      (->> resultset
           (mapcat (fn [result]
                     (let [param-ctx (assoc param-ctx :result result)]
                       (concat (->> (get repeating-anchors-lookup nil)
                                    (mapcat #(recurse-request % param-ctx)))
                               (->> find-elements
                                    (mapcat (fn [find-element]
                                              (let [entity (get result (:find-element/name find-element))
                                                    param-ctx (assoc param-ctx :entity entity)]
                                                (concat
                                                  (->> (get repeating-anchors-lookup (:find-element/name find-element))
                                                       (mapcat #(recurse-request % param-ctx)))
                                                  ; field/options-anchor requests (require :result and :entity in param-ctx)
                                                  (form-option-requests (:find-element/form find-element) param-ctx))))))))))))))


(defn requests-for-link-query [link query-params {:keys [peer] :as param-ctx} recurse?]
  (let [link-query (:link/request link)
        q (some-> link-query :link-query/value reader/read-string)
        params-map (merge query-params (q-util/build-dbhole-lookup link-query))
        param-ctx (assoc param-ctx :query-params query-params)]
    (let [request (q-util/query-value q link-query params-map param-ctx)]
      (concat
        [request]
        (->> (get-in link [:link/request :link-query/find-element])
             (mapv :find-element/connection)
             (mapv schema-util/schema-request))
        (if recurse?
          (if-let [resultset (exception/extract (hc/hydrate peer request) nil)]
            (dependent-requests resultset (:link-query/find-element link-query) (:link/anchor link) param-ctx)))))))


(defn requests-for-link-entity [link query-params {:keys [peer] :as param-ctx} recurse?]
  (let [request (q-util/->entityRequest (:link/request link) query-params)]
    (concat
      [request
       (schema-util/schema-request (get-in link [:link/request :link-entity/connection]))]
      (if recurse?
        (if-let [resultset (exception/extract (hc/hydrate peer request) nil)]
          (let [resultset (->> (if (map? resultset) [resultset] resultset)
                               (mapv #(assoc {} "entity" %)))
                find-elements [{:find-element/name "entity"
                                :find-element/form (get-in link [:link/request :link-entity/form])}]]
            (dependent-requests resultset find-elements (:link/anchor link) param-ctx)))))))


(defn requests-for-link [link query-params param-ctx recurse?]
  (let [param-ctx (assoc param-ctx :query-params query-params)]
    (case (links/link-type link)
      :link-query (requests-for-link-query link query-params param-ctx recurse?) ; Todo - hydrate refs deeper if no option link
      :link-entity (requests-for-link-entity link query-params param-ctx recurse?) ; hydrate refs deeper if no option link
      nil                                                   ; this case does not request the schema, as we don't have a connection for the link.
      )))


(defn request [params-map param-ctx recurse?]
  (if (system-links/system-link? (:link-dbid params-map))
    (let [system-link-id (-> params-map :link-dbid :id)
          system-link-request (system-links/request-for-system-link system-link-id)]
      (concat
        [system-link-request]
        (if-let [system-link-deps (-> (hc/hydrate (:peer param-ctx) system-link-request) ; ?
                                      (exception/extract nil))]
          (let [link (system-links/generate-system-link system-link-id system-link-deps)]
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
        ; todo doubtful this works on :entity-dbid-s
        (update :query-params #(util/map-values replace-tempid %)))))
