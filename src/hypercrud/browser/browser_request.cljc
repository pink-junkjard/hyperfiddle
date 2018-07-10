(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.data :refer [map-values unwrap]]
            [contrib.pprint :refer [pprint-str]]
            [contrib.reactive :as r]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.popovers :as popovers]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]
            [hyperfiddle.data :as hf]
            [hyperfiddle.runtime :as runtime]))


(declare request-from-route)
(declare request-from-link)

(defn recurse-request [link ctx]
  (if (:link/managed? link)
    (let [route' (routing/build-route' link ctx)
          popover-id (popovers/popover-id link ctx)]
      (if @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :popovers popover-id])
        ; if the anchor IS a popover, we need to run the same logic as link/managed-popover-body
        ; the ctx needs to be updated (branched, etc), but NOT BEFORE determining the route
        ; that MUST happen in the parent context
        (let [ctx (assoc ctx :branch (popovers/branch ctx link))]
          (either/branch route'
                         (constantly nil)
                         #(request-from-route % ctx)))))
    ; if the anchor IS NOT a popover, this should be the same logic as widget/render-inline-anchors
    (request-from-link link ctx)))

(defn head-field [relative-path ctx & _]                    ; params noisey because data/form has crap for UI
  (let [ctx (context/focus ctx (cons :head relative-path))] ; todo :head links should fall out with link/class
    (->> @(:hypercrud.browser/links ctx)
         (remove :link/dependent?)
         (filter (link/same-path-as? (:hypercrud.browser/path ctx)))
         (mapcat #(recurse-request % ctx)))))

(defn body-field [relative-path ctx & _]                    ; params noisey because data/form has crap for UI
  (let [ctx (context/focus ctx relative-path)]              ; todo nested fields
    (->> @(:hypercrud.browser/links ctx)
         (filter :link/dependent?)
         (filter (link/same-path-as? (:hypercrud.browser/path ctx)))
         (mapcat #(recurse-request % ctx)))))

(defn- filter-inline [links] (filter :link/render-inline? links))

(defn fiddle-requests [ctx]
  ; at this point we only care about inline links
  (let [ctx (update ctx :hypercrud.browser/links (partial r/fmap filter-inline))]
    (concat
      (->> @(:hypercrud.browser/links ctx)
           (remove :link/dependent?)
           (filter (link/same-path-as? []))
           (mapcat #(recurse-request % ctx)))
      (condp = (:hypercrud.browser/data-cardinality ctx)
        :db.cardinality/one (->> ctx
                                 (hf/form (fn [path ctx & _]
                                            (concat (head-field path (context/focus ctx [:head]))
                                                    (body-field path (context/focus ctx [:body])))))
                                 flatten)
        :db.cardinality/many (concat
                               (->> (hf/form head-field (context/focus ctx [:head]))
                                    (flatten))
                               (->> (r/unsequence (:hypercrud.browser/data ctx)) ; the request side does NOT need the cursors to be equiv between loops
                                    (mapcat (fn [[relation i]]
                                              (->> (context/body ctx relation)
                                                   (hf/form body-field)
                                                   flatten)))))
        ; blank fiddles
        nil)
      (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
        ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
        ; EntityRequest args are too structured.
        (let [[_ [inner-fiddle & inner-args]] (:route ctx)]
          (request-from-route [inner-fiddle (vec inner-args)] ctx))))))

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
                           (cats/fmap fiddle-requests)
                           unwrap)))))))))

(defn request-from-link [link ctx]
  (unwrap (base/from-link link ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))))
