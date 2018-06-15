(ns hypercrud.browser.browser-request
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either]
            [contrib.data :refer [unwrap]]
            [contrib.reactive :as r]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.popovers :as popovers]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.schema :as schema-util]
            [hyperfiddle.runtime :as runtime]))


(declare request-from-route)
(declare request-from-link)

(defn recurse-request [link ctx]
  (if (:link/managed? link)
    (let [route' (routing/build-route' link (context/legacy-ctx ctx)) ; Set legacy because bypassing build-link-props
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

(defn cell-dependent-requests [ctx]
  (let [ctx (context/legacy-cell-data ctx)]
    (concat
      (->> @(:hypercrud.browser/links ctx)
           (filter :link/dependent?)
           (filter (link/same-path-as? [(:fe-pos ctx)]))
           (mapcat #(recurse-request % ctx)))
      (->> (r/cursor (:hypercrud.browser/find-element ctx) [:fields])
           (r/unsequence :id)
           (mapcat (fn [[field id]]
                     (let [ctx (context/field ctx @field)
                           ctx (context/legacy-value ctx)]
                       (->> @(:hypercrud.browser/links ctx)
                            (filter :link/dependent?)
                            (filter (link/same-path-as? [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]))
                            (mapcat #(recurse-request % ctx))))))))))

(defn form-requests [ctx]                                   ; ui equivalent of form
  (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx))
       (mapcat (fn [[fe i]]
                 (cell-dependent-requests (context/set-find-element ctx i))))))

(defn table-requests [ctx]                                  ; ui equivalent of table
  ; the request side does NOT need the cursors to be equiv between loops
  (->> (r/unsequence (:relations ctx))
       (mapcat (fn [[relation i]]
                 (form-requests (context/relation ctx relation))))))

(defn- filter-inline [links] (filter :link/render-inline? links))

(defn fiddle-dependent-requests [ctx]
  ; at this point we only care about inline links
  (let [ctx (update ctx :hypercrud.browser/links (partial r/fmap filter-inline))]
    (concat
      (->> @(:hypercrud.browser/links ctx)
           (remove :link/dependent?)
           (filter (link/same-path-as? []))
           (mapcat #(recurse-request % ctx)))
      (->> (r/unsequence (:hypercrud.browser/ordered-fes ctx)) ; might have empty results-- DJG Dont know what this prior comment means?
           (mapcat (fn [[fe i]]
                     (let [ctx (context/set-find-element ctx i)
                           fe-pos (:fe-pos ctx)]
                       (concat
                         (->> @(:hypercrud.browser/links ctx)
                              (remove :link/dependent?)
                              (filter (link/same-path-as? [fe-pos]))
                              (mapcat #(recurse-request % ctx)))
                         (->> @(r/cursor (:hypercrud.browser/find-element ctx) [:fields])
                              (mapcat (fn [field]
                                        (let [ctx (context/field ctx field)]
                                          (->> @(:hypercrud.browser/links ctx)
                                               (remove :link/dependent?)
                                               (filter (link/same-path-as? [fe-pos (:hypercrud.browser/attribute ctx)]))
                                               (mapcat #(recurse-request % ctx))))))))))))
      (cond (:relation ctx) (form-requests ctx)
            (:relations ctx) (table-requests ctx)
            :blank nil))))

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
                           (cats/fmap fiddle-dependent-requests)
                           unwrap)))))))))

(defn request-from-link [link ctx]
  (unwrap (base/from-link link ctx (fn [route ctx]
                                     (either/right (request-from-route route ctx))))))
