(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :as eval]))


(declare request)
(declare request-from-route)

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
      (request anchor ctx))))

(defn link-dependent-requests [result ordered-fes anchors ctx]
  (let [result (cond
                 (map? result) [result]
                 (coll? result) result
                 :else result)
        anchors (->> (if (:keep-disabled-anchors? ctx)
                       anchors
                       (remove :anchor/disabled? anchors))
                     (filter :anchor/render-inline?))       ; at this point we only care about inline anchors
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

(defn request-fn [link ctx]
  ; todo report eval and invocation errors back to the user
  (case @(:display-mode ctx)
    :user (or (some->> (or (some-> (:user-request ctx) either/right)
                           (if-not (empty? (:link/request link))
                             (eval/eval-str' (:link/request link))))
                       (cats/fmap (fn [user-fn]
                                    (fn [result ordered-fes anchors ctx]
                                      (try (user-fn result ordered-fes anchors ctx)
                                           (catch :default e
                                             (js/console.error (pr-str e))
                                             nil))))))
              (either/right link-dependent-requests))
    :xray (either/right link-dependent-requests)
    :root (either/right link-dependent-requests)))

(defn requests-for-link [link ctx]
  (-> (mlet [ordered-fes (base/get-ordered-find-elements link ctx)
             link-request (base/request-for-link link ordered-fes ctx)]
        (cats/return
          (concat
            (if link-request [link-request])
            (schema-util/schema-requests-for-link ordered-fes ctx)
            (-> (mlet [result (if link-request
                                (hc/hydrate (:peer ctx) link-request)
                                (either/right nil))
                       schemas (schema-util/hydrate-schema ordered-fes ctx)
                       f (request-fn link ctx)]
                  (base/process-results f link link-request result schemas ordered-fes ctx))
                (cats/mplus (either/right nil))
                (cats/extract)))))
      (cats/mplus (either/right nil))
      (cats/extract)))

(defn request-from-route [route ctx]
  (let [ctx (context/route ctx route)
        route (:route ctx)]
    (if (auto-link/system-link? (:link-id route))
      (let [link (auto-link/hydrate-system-link (:link-id route) ctx)]
        (requests-for-link link ctx))
      (let [meta-link-request (base/meta-request-for-link ctx)]
        (concat [meta-link-request]
                (-> (hc/hydrate (:peer ctx) meta-link-request)
                    (either/branch (constantly nil)
                                   #(requests-for-link % ctx))))))))

(defn request [anchor ctx]
  (if (:anchor/link anchor)
    (-> (anchor/build-anchor-route' anchor ctx)
        (either/branch
          (constantly nil)
          (fn [route]
            ; entire context must be encoded in the route
            (request-from-route route (context/clean ctx)))))))
