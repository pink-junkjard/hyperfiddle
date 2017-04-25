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
            [hypercrud.types :as types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.util :as util]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.browser.link-util :as link-util]))


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
  (assert link-dbid)
  (let [form-pull-exp ['*
                       {:hypercrud/owner ['*]
                        :form/field
                        ['*
                         {:field/attribute ['*
                                            {:attribute/valueType [:db/id :db/ident]
                                             :attribute/cardinality [:db/id :db/ident]
                                             :attribute/unique [:db/id :db/ident]}]}]}]]
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
                                      :anchor/find-element [:db/id :find-element/name :find-element/connection]
                                      :anchor/attribute [:db/id :attribute/ident]}]
                       :hypercrud/owner ['*]}])))

(defn merge-anchors [sys-anchors link-anchors]
  ; Merge the link-anchors into the sys-anchors such that matching anchors properly override.
  ; anchor uniqueness is determined by [repeat entity attr ident]. Nil ident means
  ; don't match anything. For example [nil nil nil nil] can just mean i have a lot
  ; of top level links that i didn't bother to name yet.
  (let [f (fn [e]
            [(or (-> e :anchor/repeating?) false)           ; nil matches false
             (-> e :anchor/find-element :db/id)             ; nil is okay here
             (-> e :anchor/attribute :db/id)                ; nil is okay here
             (or (-> e :anchor/ident) (:db/id e)) #_"if no ident, it's unique"])
        collated (merge-with concat (group-by f sys-anchors) (group-by f link-anchors))
        merged (map #(apply merge %) (vals collated)) #_(apply map merge (vals collated))]
    merged))

(defn hydrate-link [peer link-dbid param-ctx]
  (if (system-links/system-link? link-dbid)
    (let [system-link-idmap (-> link-dbid :id)]
      (->> (system-links/request-for-system-link system-link-idmap)
           (hc/hydrate peer)
           (cats/fmap (fn [parent-link]
                        (system-links/generate-system-link system-link-idmap parent-link param-ctx)))))
    (hc/hydrate peer (request-for-link link-dbid))))

(declare user-result)

(defn ui [{query-params :query-params :as params-map} param-ctx]
  (mlet [link (hydrate-link (:peer param-ctx) (:link-dbid params-map) param-ctx)
         request (try-on
                   (case (link-util/link-type link)
                     :link-query (let [link-query (:link/request link)
                                       q (some-> link-query :link-query/value reader/read-string)
                                       params-map (merge query-params (q-util/build-dbhole-lookup link-query))]
                                   (q-util/query-value q link-query params-map param-ctx))
                     :link-entity (q-util/->entityRequest (:link/request link) (:query-params params-map))
                     :link-blank nil
                     nil))
         result (if request (hc/hydrate (:peer param-ctx) request) (exception/success nil))
         ; schema is allowed to be nil if the link only has anchors and no data dependencies
         schema (exception/try-or-else (hc/hydrate (:peer param-ctx) (schema-util/schema-request nil)) nil)]
        (cats/return
          (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                param-ctx (assoc param-ctx                  ; provide defaults before user-bindings run. TODO query side
                            :query-params query-params
                            :schema indexed-schema
                            :read-only (or (:read-only param-ctx) (constantly false)))

                ; ereq doesn't have a fe yet; wrap with a fe.
                ; Doesn't make sense to do on server since this is going to optimize away anyway.

                result (cond

                         (instance? types/EntityRequest request) ; But the ereq might return a vec for cardinality many
                         (cond
                           ; order matters here a lot!
                           (nil? result) nil
                           (empty? result) (if (coll? (.-dbid-s request)) [] {}) ; comes back as [] sometimes if cardinaltiy many request. this is causing problems as nil or {} in different places.
                           (map? result) {"entity" result}
                           (coll? result) (mapv (fn [relation] {"entity" relation}) result))


                         (instance? types/QueryRequest request)
                         (cond
                           (-> link :link/request :link-query/single-result-as-entity?) (first result)
                           :else result))

                colspec (form-util/determine-colspec result link param-ctx)
                system-anchors (if-not (system-links/system-link? (-> params-map :link-dbid))
                                 (system-links/system-anchors link result param-ctx))]

            (case (get param-ctx :display-mode)             ; default happens higher, it influences queries too
              :user ((user-result link param-ctx) result colspec (merge-anchors system-anchors (:link/anchor link)) (user-bindings link param-ctx))
              :xray (auto-control/result result colspec (merge-anchors system-anchors (:link/anchor link)) (user-bindings link param-ctx))
              :root (auto-control/result result colspec system-anchors param-ctx))))))

(defn safe-ui [& args]
  ; there can be a hydrate error, or a javascript error in hyperfiddle, or
  ; a js error in a user-fn.
  (try
    (let [dom-or-e (apply ui args)]
      (if (exception/failure? dom-or-e)
        [:pre (or (-> dom-or-e .-e .-data)
                  (pr-str (-> dom-or-e .-e)))]              ; hydrate error
        (.-v dom-or-e)))                                    ; happy path
    (catch :default e                                       ; js errors? Why do we need this.
      [:pre (.-stack e)])))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (let [{user-fn :value error :error} (eval-str (:link/renderer link))]
      (if error
        (fn [result colspec anchors param-ctx]
          [:pre (pprint/pprint error)])
        (fn [result colspec anchors param-ctx]
          [safe-user-renderer user-fn result colspec anchors param-ctx])))))

(defn user-result [link param-ctx]
  ; only need a safewrap on other people's user-fns; this context's user fn only needs the topmost safewrap.
  (let [user-fn (first (remove nil? [(:user-renderer param-ctx) (link-user-fn link) auto-control/result]))]
    (fn [result colspec anchors param-ctx]
      (let [anchor-index (->> anchors
                              (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                              (into {}))
            with-inline-result (fn [ident param-ctx f]
                                 (let [anchor (get anchor-index ident)
                                       params-map (links/build-url-params-map anchor param-ctx)]
                                   @(ui params-map (assoc param-ctx :user-renderer f))))
            param-ctx (assoc param-ctx
                        :link-fn (fn [ident label param-ctx]
                                   (let [anchor (get anchor-index ident)
                                         props (-> (links/build-link-props anchor param-ctx)
                                                   #_(dissoc :style) #_"custom renderers don't want colored links")]
                                     [(:navigate-cmp param-ctx) props label]))
                        :with-inline-result with-inline-result
                        )]
        ; result is relation or set of relations
        (user-fn result colspec anchors param-ctx)))))


(declare request)

(defn link-query-dependent-requests [result find-elements anchors param-ctx]
  (let [anchors (filter :anchor/render-inline? anchors)     ; at this point we only care about inline anchors
        ; sys links already merged and accounted for above
        ; manufacture the query-params
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (-> anchor :anchor/repeating?)
                                              fe (-> anchor :anchor/find-element :find-element/name)
                                              attr (-> anchor :anchor/attribute :attribute/ident)
                                              fe (if (and attr (nil? fe)) "entity" fe)] ; This should be auto-selected in the future so we always have fe if we have attr.
                                          [r fe attr]))))
        recurse-request (fn [anchor param-ctx]
                          (let [params-map (links/build-url-params-map anchor param-ctx)
                                param-ctx (-> param-ctx
                                              (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                              (dissoc :result :entity :attribute :value))]
                            (request params-map param-ctx)))]

    ; param-ctx maintains: :result, :entity, :attribute, :value
    (let [lookup {:index #(get anchors-lookup [true nil nil]) ; why repeating? have we been filtered higher? doesn't seem so.
                  :index-new #(get anchors-lookup [false nil nil]) ; New Page?
                  :relation (constantly [])                 ; Relation links don't make sense to me yet. where would they go?
                  :relation-new (constantly [])             ; We haven't implemented them.
                  :entity #(get anchors-lookup [true (:find-element/name %1) nil])
                  :entity-new #(get anchors-lookup [false (:find-element/name %1) nil])
                  :entity-attr #(get anchors-lookup [true (:find-element/name %1) (:attribute/ident %2)])
                  :entity-attr-new #(get anchors-lookup [false (:find-element/name %1) (:attribute/ident %2)])}]
      (concat
        (->> ((:index lookup)) (mapcat #(recurse-request % param-ctx)))
        (->> ((:index-new lookup)) (mapcat #(recurse-request % param-ctx)))
        (->> result
             (mapcat (fn [relation]
                       (let [param-ctx (assoc param-ctx :result relation)]
                         (concat (->> ((:relation lookup)) (mapcat #(recurse-request % param-ctx)))
                                 (->> ((:relation-new lookup)) (mapcat #(recurse-request % param-ctx)))
                                 (->> find-elements         ; these are wrong for link-entity somehow?
                                      (mapcat (fn [fe]
                                                (let [entity (get relation (:find-element/name fe))
                                                      param-ctx (assoc param-ctx :entity entity)]
                                                  (concat (->> ((:entity lookup) fe) (mapcat #(recurse-request % param-ctx)))
                                                          (->> ((:entity-new lookup) fe) (mapcat #(recurse-request % param-ctx)))
                                                          (->> (-> fe :find-element/form :form/field)
                                                               (mapcat (fn [field]
                                                                         (let [attribute (-> field :field/attribute)
                                                                               param-ctx (assoc param-ctx :attribute attribute
                                                                                                          :value (get entity (:attribute/ident attribute)))]
                                                                           (concat
                                                                             (->> ((:entity-attr lookup) fe attribute) (mapcat #(recurse-request % param-ctx)))
                                                                             (->> ((:entity-attr-new lookup) fe attribute) (mapcat #(recurse-request % param-ctx))))
                                                                           ))))))))))))))))))

(defn link-entity-dependent-requests [result form anchors param-ctx]
  (let [recurse-request (fn [anchor param-ctx]
                          (let [params-map (links/build-url-params-map anchor param-ctx)
                                param-ctx (-> param-ctx
                                              (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                              (dissoc :result :entity :attribute :value))]
                            (request params-map param-ctx)))
        anchors (filter :anchor/render-inline? anchors)
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (-> anchor :anchor/repeating?)
                                              attr (-> anchor :anchor/attribute :attribute/ident)]
                                          [r attr]))))]
    (let [lookup {:index #(get anchors-lookup [true nil])   ; why repeating? have we been filtered higher? doesn't seem so.
                  :index-new #(get anchors-lookup [false nil]) ; New Page?
                  :entity-attr #(get anchors-lookup [true (:attribute/ident %)])
                  :entity-attr-new #(get anchors-lookup [false (:attribute/ident %)])}]
      (concat
        (->> ((:index lookup)) (mapcat #(recurse-request % param-ctx)))
        (->> ((:index-new lookup)) (mapcat #(recurse-request % param-ctx)))
        (->> result
             (mapcat (fn [relation]
                       (let [entity (get relation "entity")
                             param-ctx (assoc param-ctx :entity entity)]
                         (->> (-> form :form/field)
                              (mapcat (fn [field]
                                        (let [attribute (-> field :field/attribute)
                                              param-ctx (assoc param-ctx :attribute attribute
                                                                         :value (get entity (:attribute/ident attribute)))]
                                          (concat
                                            (->> ((:entity-attr lookup) attribute) (mapcat #(recurse-request % param-ctx)))
                                            (->> ((:entity-attr-new lookup) attribute) (mapcat #(recurse-request % param-ctx))))
                                          ))))))))))))


(defn requests-for-link-query [link query-params param-ctx]
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
        (exception/extract
          (mlet [result (hc/hydrate (:peer param-ctx) request)
                 schema (hc/hydrate (:peer param-ctx) (schema-util/schema-request nil))] ; map connections
                (cats/return
                  (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                        param-ctx (assoc param-ctx :schema indexed-schema)]
                    (link-query-dependent-requests result (:link-query/find-element link-query)
                                                   (merge-anchors
                                                     (system-links/system-anchors link result param-ctx)
                                                     (:link/anchor link))
                                                   param-ctx))))
          nil)))))

(defn requests-for-link-entity [link query-params param-ctx]
  (let [request (q-util/->entityRequest (:link/request link) query-params)
        schema-request (schema-util/schema-request (get-in link [:link/request :link-entity/connection]))]
    (concat
      [request schema-request]
      (exception/extract
        (mlet [result (hc/hydrate (:peer param-ctx) request)
               schema (hc/hydrate (:peer param-ctx) schema-request)]
              (cats/return
                (let [indexed-schema (->> (mapv #(get % "?attr") schema) (util/group-by-assume-unique :attribute/ident))
                      param-ctx (assoc param-ctx :schema indexed-schema)
                      result (->> (if (map? result) [result] result) (mapv #(assoc {} "entity" %)))
                      form (-> (-> link :link/request :link-entity/form)
                               (update :form/field form-util/filter-visible-fields param-ctx))]
                  (link-entity-dependent-requests result form
                                                  (merge-anchors
                                                    (system-links/system-anchors link result param-ctx)
                                                    (:link/anchor link))
                                                  param-ctx))))
        nil))))

(defn requests-for-link [link query-params param-ctx]
  (let [param-ctx (assoc param-ctx :query-params query-params)]
    (case (link-util/link-type link)
      :link-query (requests-for-link-query link query-params param-ctx)
      :link-entity (requests-for-link-entity link query-params param-ctx)
      :link-blank (link-query-dependent-requests [] [] (:link/anchor link) param-ctx)))) ; this case does not request the schema, as we don't have a connection for the link.


(defn request [params-map param-ctx]
  (if (system-links/system-link? (:link-dbid params-map))
    (let [system-link-idmap (-> params-map :link-dbid :id)
          system-link-request (system-links/request-for-system-link system-link-idmap)]
      (concat
        [system-link-request]
        (if-let [system-link-deps (-> (hc/hydrate (:peer param-ctx) system-link-request) ; ?
                                      (exception/extract nil))]
          (let [link (system-links/generate-system-link system-link-idmap system-link-deps param-ctx)]
            (requests-for-link link (:query-params params-map) param-ctx)))))
    (let [link-request (request-for-link (:link-dbid params-map))]
      (concat [link-request]
              (if-let [link (-> (hc/hydrate (:peer param-ctx) link-request) (exception/extract nil))]
                (requests-for-link link (:query-params params-map) param-ctx))))))


(defn replace-tempids-in-params-map [tempid-lookup params-map]
  (let [replace-tempid #(or (get tempid-lookup %) %)]
    (-> params-map
        (update :link-dbid replace-tempid)
        ; todo doubtful this works on :entity-dbid-s (now :entity)
        (update :query-params #(util/map-values replace-tempid %)))))
