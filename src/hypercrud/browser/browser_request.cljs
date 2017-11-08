(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.client.schema :as schema-util]))


(declare request-from-route)
(declare request-from-anchor)

(defn recurse-request [anchor ctx]
  (if (:anchor/managed? anchor)
    (let [route' (anchor/build-anchor-route' anchor ctx)
          ctx (context/anchor-branch ctx anchor)]
      (if (get-in (-> ctx :peer .-state-atom deref) [:popovers (:branch ctx)])
        ; if the anchor IS a popover, we need to run the same logic as anchor/build-anchor-props
        ; the ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
        ; that MUST happen in the parent context
        (let [ctx (-> ctx
                      (context/clean)
                      (update :debug #(str % ">popover-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]")))]
          (either/branch route'
                         (constantly nil)
                         #(request-from-route % ctx)))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (let [ctx (update ctx :debug #(str % ">inline-link[" (:db/id anchor) ":" (or (:anchor/ident anchor) (:anchor/prompt anchor)) "]"))]
      (request-from-anchor anchor ctx))))

(defn link-dependent-requests [result ordered-fes anchors ctx]
  (let [result (cond
                 (map? result) [result]
                 (coll? result) result
                 :else result)
        anchors (filter :anchor/render-inline? anchors)     ; at this point we only care about inline anchors
        anchors-lookup (->> anchors
                            (group-by (fn [anchor]
                                        (let [r (or (-> anchor :anchor/repeating?) false)
                                              fe (-> anchor :anchor/find-element :find-element/name)
                                              attr (-> anchor :anchor/attribute)]
                                          [r fe attr]))))
        lookup {:index #(get anchors-lookup [false nil nil])
                :relation (constantly [])                   ; Relation links don't make sense to me yet. where would they go?
                :relation-new (constantly [])               ; We haven't implemented them.
                :entity-t #(get anchors-lookup [true (:find-element/name %1) nil])
                :entity-f #(get anchors-lookup [false (:find-element/name %1) nil])
                :entity-attr-t #(get anchors-lookup [true (:find-element/name %1) %2])
                :entity-attr-f #(get anchors-lookup [false (:find-element/name %1) %2])}]
    (concat
      (->> ((:index lookup)) (mapcat #(recurse-request % ctx)))
      (->> ((:relation-new lookup)) (mapcat #(recurse-request % ctx)))
      (->> ordered-fes                                      ; might have empty results
           (mapcat (fn [fe]
                     (let [ctx (context/find-element ctx fe)]
                       (concat
                         (->> ((:entity-f lookup) fe) (mapcat #(recurse-request % ctx)))
                         (->> (get-in fe [:find-element/form :form/field])
                              (mapcat (fn [{:keys [:field/attribute]}]
                                        (let [ctx (context/attribute ctx attribute)]
                                          (->> ((:entity-attr-f lookup) fe attribute) (mapcat #(recurse-request % ctx))))))))))))
      (->> result
           (mapcat (fn [relation]
                     (let [ctx (context/relation ctx relation)]
                       (concat (->> ((:relation lookup)) (mapcat #(recurse-request % ctx)))
                               (->> ordered-fes
                                    (mapcat (fn [fe]
                                              (let [entity (get relation (:find-element/name fe))
                                                    ctx (-> ctx
                                                            (context/find-element fe)
                                                            (context/entity entity))]
                                                (concat
                                                  (->> ((:entity-t lookup) fe) (mapcat #(recurse-request % ctx)))
                                                  (->> (get-in fe [:find-element/form :form/field])
                                                       (mapcat (fn [{:keys [:field/attribute]}]
                                                                 (let [ctx (-> (context/attribute ctx attribute)
                                                                               (context/value (get entity attribute)))]
                                                                   (->> ((:entity-attr-t lookup) fe attribute) (mapcat #(recurse-request % ctx))))))))
                                                ))))))))))))

(defn f-mode-config []
  {:from-ctx :user-request
   :from-link :fiddle/request
   :with-user-fn (fn [user-fn]
                   (fn [result ordered-fes anchors ctx]
                     ; todo report invocation errors back to the user
                     (try (user-fn result ordered-fes anchors ctx)
                          (catch :default e
                            (js/console.error (pr-str e))
                            nil))))
   :default link-dependent-requests})

(defn process-data [{:keys [result ordered-fes anchors ctx]}]
  (mlet [request-fn (base/fn-from-mode (f-mode-config) (:fiddle ctx) ctx)]
    (cats/return (request-fn result ordered-fes anchors ctx))))

(defn request-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [meta-link-req' link']} (base/hydrate-link ctx)]
    (concat (if-let [meta-link-req (-> meta-link-req'
                                       (cats/mplus (either/right nil))
                                       (cats/extract))]
              [meta-link-req])
            (-> (mlet [link link'
                       ordered-fes (base/get-ordered-find-elements link ctx)
                       link-request (base/request-for-link link ordered-fes ctx)]
                  (cats/return
                    (concat
                      (if link-request [link-request])
                      (schema-util/schema-requests-for-link ordered-fes ctx)
                      (-> (base/process-results link link-request ordered-fes ctx)
                          (cats/bind process-data)
                          (cats/mplus (either/right nil))
                          (cats/extract)))))
                (cats/mplus (either/right nil))
                (cats/extract)))))

(defn request-from-anchor [anchor ctx]
  (-> (base/from-anchor anchor ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))
      (cats/mplus (either/right nil))
      (cats/extract)))
