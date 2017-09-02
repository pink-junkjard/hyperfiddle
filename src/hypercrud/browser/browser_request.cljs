(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]))


(declare request)
(declare request')

(defn recurse-request [anchor param-ctx]
  (if (:anchor/managed? anchor)
    (if (get-in (-> param-ctx :peer .-state-atom deref) [:popovers (:branch param-ctx) (-> anchor :db/id :id)])
      ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
      ; the param-ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
      ; that MUST happen in the parent context
      (let [route' (anchor/build-anchor-route' anchor param-ctx)
            param-ctx (-> (context/anchor-branch param-ctx anchor)
                          (context/clean)
                          (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]")))]
        (either/branch route'
                       (constantly nil)
                       #(request' % param-ctx))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [param-ctx (update param-ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))]
      (request anchor param-ctx))))

(defn link-dependent-requests [result colspec anchors param-ctx]
  (let [result (cond
                 (map? result) [result]
                 (coll? result) result
                 :else result)
        anchors (->> anchors
                     (remove :anchor/disabled?)
                     (filter :anchor/render-inline?))       ; at this point we only care about inline anchors
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (or (-> anchor :anchor/repeating?) false)
                                              fe (-> anchor :anchor/find-element :find-element/name)
                                              attr (-> anchor :anchor/attribute)]
                                          [r fe attr]))))
        find-elements (->> (partition 4 colspec)
                           (map (fn [[dbval fe attr maybe-field]] fe))
                           (distinct))
        lookup {:index #(get anchors-lookup [false nil nil])
                :relation (constantly [])                   ; Relation links don't make sense to me yet. where would they go?
                :relation-new (constantly [])               ; We haven't implemented them.
                :entity-t #(get anchors-lookup [true (:find-element/name %1) nil])
                :entity-f #(get anchors-lookup [false (:find-element/name %1) nil])
                :entity-attr-t #(get anchors-lookup [true (:find-element/name %1) %2])
                :entity-attr-f #(get anchors-lookup [false (:find-element/name %1) %2])}]
    (concat
      (->> ((:index lookup)) (mapcat #(recurse-request % param-ctx)))
      (->> ((:relation-new lookup)) (mapcat #(recurse-request % param-ctx)))
      (->> find-elements                                    ; might have empty results
           (mapcat (fn [fe]
                     (let [param-ctx (context/find-element param-ctx fe)]
                       (concat
                         (->> ((:entity-f lookup) fe) (mapcat #(recurse-request % param-ctx)))
                         (->> (get-in fe [:find-element/form :form/field])
                              (mapcat (fn [field]
                                        (let [attr-ident (-> field :field/attribute)
                                              param-ctx (context/attribute param-ctx (get-in param-ctx [:schema attr-ident]))]
                                          (->> ((:entity-attr-f lookup) fe attr-ident) (mapcat #(recurse-request % param-ctx))))))))))))
      (->> result
           (mapcat (fn [relation]
                     (let [param-ctx (context/relation param-ctx relation)]
                       (concat (->> ((:relation lookup)) (mapcat #(recurse-request % param-ctx)))
                               (->> find-elements
                                    (mapcat (fn [fe]
                                              (let [entity (get relation (:find-element/name fe))
                                                    param-ctx (-> param-ctx
                                                                  (context/find-element fe)
                                                                  (context/entity entity))]
                                                (concat
                                                  (->> ((:entity-t lookup) fe) (mapcat #(recurse-request % param-ctx)))
                                                  (->> (get-in fe [:find-element/form :form/field])
                                                       (mapcat (fn [field]
                                                                 (let [attr-ident (-> field :field/attribute)
                                                                       param-ctx (-> (context/attribute param-ctx (get-in param-ctx [:schema attr-ident]))
                                                                                     (context/value (get entity attr-ident)))]
                                                                   (->> ((:entity-attr-t lookup) fe attr-ident) (mapcat #(recurse-request % param-ctx))))))))
                                                ))))))))))))

(defn requests-for-link [link query-params param-ctx]
  (-> (->> (base/request-for-link link query-params param-ctx)
           (cats/fmap (fn [link-request]
                        (concat
                          (if link-request [link-request])
                          (schema-util/schema-requests-for-link link query-params param-ctx)
                          (-> (mlet [result (if link-request
                                              (hc/hydrate (:peer param-ctx) link-request)
                                              (either/right nil))
                                     schemas (schema-util/hydrate-schema link query-params param-ctx)]
                                (base/process-results (constantly link-dependent-requests) query-params link link-request result schemas param-ctx))
                              (cats/mplus (either/right nil))
                              (cats/extract))))))
      (cats/mplus (either/right nil))
      (cats/extract)))

(defn request' [route param-ctx]
  (let [param-ctx (context/route param-ctx route)]
    (if (auto-link/system-link? (:link-dbid route))
      (let [system-link-idmap (-> route :link-dbid :id)
            system-link-requests (auto-link/request-for-system-link system-link-idmap param-ctx)]
        (concat
          (remove nil? system-link-requests)
          (-> (mapv #(if % (hc/hydrate (:peer param-ctx) %) (either/right nil)) system-link-requests)
              (cats/sequence)
              (either/branch
                (constantly nil)
                (fn [system-link-deps]
                  (let [link (auto-link/hydrate-system-link system-link-idmap system-link-deps param-ctx)]
                    (requests-for-link link (:query-params route) param-ctx)))))))
      (let [meta-link-request (base/meta-request-for-link (:link-dbid route) param-ctx)]
        (concat [meta-link-request]
                (-> (hc/hydrate (:peer param-ctx) meta-link-request)
                    (either/branch (constantly nil)
                                   #(requests-for-link % (:query-params route) param-ctx))))))))

(defn request [anchor param-ctx]
  (if (:anchor/link anchor)
    (-> (anchor/build-anchor-route' anchor param-ctx)
        (either/branch
          (constantly nil)
          (fn [route]
            (request' route
                      ; entire context must be encoded in the route
                      (context/clean param-ctx)))))))
