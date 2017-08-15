(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]))


(declare request)
(declare request')

(defn recurse-request [anchor param-ctx]
  (if (:anchor/managed? anchor)
    ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
    ; the param-ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
    ; that MUST happen in the parent context
    (let [route' (anchor/build-anchor-route' anchor param-ctx)
          param-ctx (-> (anchor/anchor-branch-logic anchor param-ctx)
                        (dissoc :result :db :find-element :entity :attribute :value :layout :field)
                        (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]")))]
      (either/branch route'
                     (constantly nil)
                     #(request' % param-ctx)))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [param-ctx (update param-ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (:anchor/prompt anchor) "]"))]
      (request anchor param-ctx))))

(defn link-dependent-requests [result colspec anchors param-ctx]
  (let [result (cond
                 (map? result) [result]
                 (coll? result) result
                 :else result)
        anchors (filter :anchor/render-inline? anchors)     ; at this point we only care about inline anchors
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (or (-> anchor :anchor/repeating?) false)
                                              fe (-> anchor :anchor/find-element :find-element/name)
                                              attr (-> anchor :anchor/attribute :attribute/ident)]
                                          [r fe attr]))))
        find-elements (->> (partition 4 colspec)
                           (map (fn [[dbval fe attr maybe-field]] fe))
                           (distinct))
        lookup {:index #(get anchors-lookup [false nil nil])
                :relation (constantly [])                   ; Relation links don't make sense to me yet. where would they go?
                :relation-new (constantly [])               ; We haven't implemented them.
                :entity-t #(get anchors-lookup [true (:find-element/name %1) nil])
                :entity-f #(get anchors-lookup [false (:find-element/name %1) nil])
                :entity-attr-t #(get anchors-lookup [true (:find-element/name %1) (:attribute/ident %2)])
                :entity-attr-f #(get anchors-lookup [false (:find-element/name %1) (:attribute/ident %2)])}]
    (concat
      (->> ((:index lookup)) (mapcat #(recurse-request % param-ctx)))
      (->> ((:relation-new lookup)) (mapcat #(recurse-request % param-ctx)))
      (->> find-elements                                    ; might have empty results
           (mapcat (fn [fe]
                     (let [param-ctx (assoc param-ctx
                                       :db (let [conn-id (-> fe :find-element/connection :db/id :id)]
                                             (hc/db (:peer param-ctx) conn-id (get-in param-ctx [:branches conn-id])))
                                       :find-element fe)]
                       (concat
                         (->> ((:entity-f lookup) fe) (mapcat #(recurse-request % param-ctx)))
                         (->> (get-in fe [:find-element/form :form/field])
                              (mapcat (fn [field]
                                        (let [attribute (-> field :field/attribute)
                                              param-ctx (assoc param-ctx :attribute attribute)]
                                          (->> ((:entity-attr-f lookup) fe attribute) (mapcat #(recurse-request % param-ctx))))))))))))
      (->> result
           (mapcat (fn [relation]
                     (let [param-ctx (assoc param-ctx :result relation)]
                       (concat (->> ((:relation lookup)) (mapcat #(recurse-request % param-ctx)))
                               (->> find-elements
                                    (mapcat (fn [fe]
                                              (let [entity (get relation (:find-element/name fe))
                                                    param-ctx (assoc param-ctx :db (let [conn-id (-> fe :find-element/connection :db/id :id)]
                                                                                     (hc/db (:peer param-ctx) conn-id (get-in param-ctx [:branches conn-id])))
                                                                               :find-element fe
                                                                               :entity entity)]
                                                (concat
                                                  (->> ((:entity-t lookup) fe) (mapcat #(recurse-request % param-ctx)))
                                                  (->> (get-in fe [:find-element/form :form/field])
                                                       (mapcat (fn [field]
                                                                 (let [attribute (-> field :field/attribute)
                                                                       param-ctx (assoc param-ctx :attribute attribute
                                                                                                  :value (get entity (:attribute/ident attribute)))]
                                                                   (->> ((:entity-attr-t lookup) fe attribute) (mapcat #(recurse-request % param-ctx))))))))
                                                ))))))))))))

(defn requests-for-link [link query-params param-ctx]
  (-> (->> (base/request-for-link link query-params param-ctx)
           (cats/fmap (fn [link-request]
                        (concat
                          (if link-request [link-request])
                          [(schema-util/schema-request (:root-db param-ctx) nil)] ; todo map connections
                          (-> (mlet [result (if link-request
                                              (hc/hydrate (:peer param-ctx) link-request)
                                              (either/right nil))
                                     schema (hc/hydrate (:peer param-ctx) (schema-util/schema-request (:root-db param-ctx) nil))]
                                (base/process-results (constantly link-dependent-requests) query-params link link-request result schema param-ctx))
                              (cats/mplus (either/right nil))
                              (cats/extract))))))
      (cats/mplus (either/right nil))
      (cats/extract)))

(defn request' [route param-ctx]
  (if (auto-link/system-link? (:link-dbid route))
    (let [system-link-idmap (-> route :link-dbid :id)
          system-link-requests (auto-link/request-for-system-link (:root-db param-ctx) system-link-idmap)]
      (concat
        (remove nil? system-link-requests)
        (-> (mapv #(if % (hc/hydrate (:peer param-ctx) %) (either/right nil)) system-link-requests)
            (cats/sequence)
            (either/branch
              (constantly nil)
              (fn [system-link-deps]
                (let [link (auto-link/hydrate-system-link system-link-idmap system-link-deps param-ctx)]
                  (requests-for-link link (:query-params route) param-ctx)))))))
    (let [meta-link-request (base/meta-request-for-link (:root-db param-ctx) (:link-dbid route))]
      (concat [meta-link-request]
              (-> (hc/hydrate (:peer param-ctx) meta-link-request)
                  (either/branch (constantly nil)
                                 #(requests-for-link % (:query-params route) param-ctx)))))))

(defn request [anchor param-ctx]
  (if (:anchor/link anchor)
    (-> (anchor/build-anchor-route' anchor param-ctx)
        (either/branch
          (constantly nil)
          (fn [route]
            (request' route
                      ; entire context must be encoded in the route
                      (dissoc param-ctx :result :db :find-element :entity :attribute :value)))))))
