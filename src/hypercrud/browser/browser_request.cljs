(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception]
            [cljs.reader :as reader]
            [hypercrud.browser.auto-anchor :as browser-anchors]
            [hypercrud.browser.auto-link :as system-links]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.link-util :as link-util]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.form.q-util :as q-util]
            [hypercrud.types :as types :refer [->DbId ->DbVal ->EntityRequest]]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util :as util]))


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
    (->EntityRequest link-dbid nil (->DbVal hc/*root-conn-id* nil)
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
                                              fe (if (and attr (nil? fe)) "entity" fe)]
                                          [r fe attr]))))
        recurse-request (fn [anchor param-ctx]
                          (let [params-map (anchor/build-anchor-route anchor param-ctx)
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
                          (let [params-map (anchor/build-anchor-route anchor param-ctx)
                                param-ctx (-> param-ctx
                                              (update :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))
                                              (dissoc :result :entity :attribute :value))]
                            (request params-map param-ctx)))
        anchors (filter :anchor/render-inline? anchors)
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (-> anchor :anchor/repeating?)
                                              attr (-> anchor :anchor/attribute :attribute/ident)]
                                          [r nil attr]))))]

    ; Can't differentiate between index and entity links right now, bugs here.
    (let [lookup {:index #(get anchors-lookup [true nil nil])
                  :index-new #(get anchors-lookup [false nil nil])
                  :relation (constantly [])
                  :relation-new (constantly [])
                  :entity #(get anchors-lookup [true nil nil])
                  :entity-new #(get anchors-lookup [false nil nil]) ; New Page?
                  :entity-attr #(get anchors-lookup [true nil (:attribute/ident %)])
                  :entity-attr-new #(get anchors-lookup [false nil (:attribute/ident %)])}]
      (concat
        ;(->> ((:index lookup)) (mapcat #(recurse-request % param-ctx)))
        ;(->> ((:index-new lookup)) (mapcat #(recurse-request % param-ctx)))
        (->> result
             (mapcat (fn [relation]
                       (concat
                         (->> ((:relation lookup)) (mapcat #(recurse-request % param-ctx)))
                         (->> ((:relation-new lookup)) (mapcat #(recurse-request % param-ctx)))
                         (let [entity (get relation "entity")
                               param-ctx (assoc param-ctx :result relation :entity entity)]
                           (concat
                             (->> ((:entity lookup) entity) (mapcat #(recurse-request % param-ctx)))
                             (->> ((:entity-new lookup) entity) (mapcat #(recurse-request % param-ctx)))
                             (->> (-> form :form/field)
                                  (mapcat (fn [field]
                                            (let [attribute (-> field :field/attribute)
                                                  param-ctx (assoc param-ctx :attribute attribute
                                                                             :value (get entity (:attribute/ident attribute)))]
                                              (concat
                                                (->> ((:entity-attr lookup) attribute) (mapcat #(recurse-request % param-ctx)))
                                                (->> ((:entity-attr-new lookup) attribute) (mapcat #(recurse-request % param-ctx))))
                                              ))))))))))))))


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
                                                   (browser-anchors/merge-anchors
                                                     (browser-anchors/auto-anchors (system-links/system-anchors link result param-ctx))
                                                     (browser-anchors/auto-anchors (:link/anchor link)))
                                                   param-ctx))))
          nil)))))

(defn requests-for-link-entity [link query-params param-ctx]
  (let [request (q-util/->entityRequest (:link/request link) query-params)
        _ (assert (get-in link [:link/request :link-entity/connection]))
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
                                                  (browser-anchors/merge-anchors
                                                    (browser-anchors/auto-anchors (system-links/system-anchors link result param-ctx))
                                                    (browser-anchors/auto-anchors (:link/anchor link)))
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
          system-link-requests (system-links/request-for-system-link system-link-idmap)]
      (concat
        (remove nil? system-link-requests)
        (if-let [system-link-deps (exception/extract
                                    (->> (mapv #(if % (hc/hydrate (:peer param-ctx) %)
                                                      (exception/success nil)) system-link-requests)
                                         (reduce #(mlet [acc %1 v %2] (cats/return (conj acc v))) (exception/success [])))
                                    nil)]
          (let [link (system-links/hydrate-system-link system-link-idmap system-link-deps param-ctx)]
            (requests-for-link link (:query-params params-map) param-ctx)))))
    (let [link-request (request-for-link (:link-dbid params-map))]
      (concat [link-request]
              (if-let [link (-> (hc/hydrate (:peer param-ctx) link-request) (exception/extract nil))]
                (requests-for-link link (:query-params params-map) param-ctx))))))