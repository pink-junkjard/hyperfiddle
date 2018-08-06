(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.data :refer [unwrap]]
            [contrib.reactive :as r]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]
            [hyperfiddle.data :as data]))


(declare requests)
(declare with-result)

(defn request-from-route [route ctx]
  (let [ctx (-> (context/clean ctx)
                (routing/route route))]
    (when-let [meta-fiddle-request (unwrap @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx)))]
      (assert (r/reactive? meta-fiddle-request))
      (concat [@meta-fiddle-request]
              (unwrap
                (mlet [fiddle @(r/apply-inner-r (r/track base/hydrate-fiddle meta-fiddle-request ctx))
                       fiddle-request @(r/apply-inner-r (r/track base/request-for-fiddle fiddle ctx))]
                  (assert (r/reactive? fiddle-request))
                  (cats/return
                    (concat
                      (some-> @fiddle-request vector)
                      (schema-util/schema-requests-for-link ctx)
                      (->> (base/process-results fiddle fiddle-request ctx)
                           (cats/fmap requests)
                           unwrap)))))))))

(defn request-from-link [link ctx]
  (unwrap (base/from-link link ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))))

(defn head-field [relative-path ctx]
  (let [ctx (context/focus ctx (cons :head relative-path))] ; todo :head links should fall out with link/class
    (->> @(:hypercrud.browser/links ctx)
         (filter (partial link/same-path-as? (:hypercrud.browser/path ctx)))
         (mapcat #(request-from-link % ctx)))))

(defn body-field [relative-path ctx]
  (let [ctx (context/focus ctx relative-path)]
    (->> @(:hypercrud.browser/links ctx)
         (filter (partial link/same-path-as? (:hypercrud.browser/path ctx)))
         (mapcat #(request-from-link % ctx))
         (concat (let [child-fields? (not (some->> (:hypercrud.browser/fields ctx) (r/fmap nil?) deref))]
                   (when (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                     (with-result ctx)))))))

(defn with-result [ctx]
  (condp = (:hypercrud.browser/data-cardinality ctx)
    :db.cardinality/one (->> ctx
                             (data/form (fn [path ctx]
                                          (concat (head-field path (context/focus ctx [:head]))
                                                  (body-field path (context/focus ctx [:body])))))
                             flatten)
    :db.cardinality/many (concat
                           (->> (data/form head-field (context/focus ctx [:head]))
                                (flatten))
                           (->> (r/unsequence (:hypercrud.browser/data ctx)) ; the request side does NOT need the cursors to be equiv between loops
                                (mapcat (fn [[relation i]]
                                          (->> (context/body ctx relation)
                                               (data/form body-field)
                                               flatten)))))
    ; blank fiddles
    nil))

(defn- filter-inline [links] (filter :link/render-inline? links))
(defn- remove-managed [links] (remove :link/managed? links))

(defn requests [ctx]
  ; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
  (let [ctx (update ctx :hypercrud.browser/links (partial r/fmap (r/comp remove-managed filter-inline)))]
    (concat
      (->> @(:hypercrud.browser/links ctx)
           (filter (partial link/same-path-as? []))
           (mapcat #(request-from-link % ctx)))
      (with-result ctx)
      (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
        ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
        ; EntityRequest args are too structured.
        (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
          (request-from-route [inner-fiddle (vec inner-args)] ctx))))))
