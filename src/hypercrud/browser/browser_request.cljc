(ns hypercrud.browser.browser-request
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.routing :as routing]
    [hypercrud.client.peer :refer [-quiet-unwrap]]
    [hyperfiddle.data :as data]
    [hyperfiddle.project :as project]))


(declare requests)
(declare with-result)

(defn request-from-route [route ctx]
  (when-let [ctx (-> (context/clean ctx)
                     (routing/route+ route)
                     -quiet-unwrap)]
    (when-let [meta-fiddle-request (-quiet-unwrap @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx)))]
      (assert (r/reactive? meta-fiddle-request))
      (concat [@meta-fiddle-request
               (project/attrs-request ctx)]
              (-quiet-unwrap
                (mlet [fiddle @(r/apply-inner-r (r/track base/hydrate-fiddle meta-fiddle-request ctx))
                       fiddle-request @(r/apply-inner-r (r/track base/request-for-fiddle fiddle ctx))]
                  (assert (r/reactive? fiddle-request))
                  (cats/return
                    (concat
                      (some-> @fiddle-request vector)
                      (->> (base/process-results fiddle fiddle-request ctx)
                           (cats/fmap requests)
                           (-quiet-unwrap))))))))))

(defn request-from-link [link ctx]
  (-quiet-unwrap (base/from-link link ctx (fn [route ctx]
                                            (either/right (request-from-route route ctx))))))

(defn body-field [ctx]
  (->> @(r/fmap :fiddle/links (:hypercrud.browser/fiddle ctx))
       (filter (partial link/same-path-as? (:hypercrud.browser/path ctx)))
       (mapcat #(request-from-link % ctx))
       (concat (let [child-fields? (not @(r/fmap (r/comp nil? ::field/children) (:hypercrud.browser/field ctx)))]
                 (when (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                   (with-result ctx))))))

(defn with-result [ctx]
  (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
    :db.cardinality/one (->> (data/form-with-naked-legacy ctx)
                             (map (fn [path]
                                    (if (seq path)          ; scar - guard infinite recursion on [] links
                                      (body-field (context/focus ctx path)))))
                             flatten)
    ; Spread across the rows and flip cardinality
    :db.cardinality/many (->> @(:hypercrud.browser/data ctx)
                              (mapcat (fn [row]
                                        ; the request side does NOT need the cursors to be equiv between loops
                                        (let [ctx (context/row ctx (r/atom row))]
                                          (->> (data/form-with-naked-legacy ctx)
                                               (map (fn [path]
                                                      (if (seq path)
                                                        (body-field (context/focus ctx path)))))
                                               flatten)))))
    ; blank fiddles
    nil))

; contains hf/portal
(defn filter-inline-links [fiddle] (update fiddle :fiddle/links #(filter (comp (partial = :hf/iframe) :link/rel) %)))

(defn requests [ctx]
  ; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
  (let [ctx (update ctx :hypercrud.browser/fiddle (partial r/fmap filter-inline-links))]
    (concat
      (->> @(r/fmap :fiddle/links (:hypercrud.browser/fiddle ctx))
           (filter (partial link/same-path-as? []))
           (mapcat #(request-from-link % ctx)))
      (with-result ctx)
      (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
        ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
        ; EntityRequest args are too structured.
        (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)]
          (request-from-route [inner-fiddle (vec inner-args)] ctx))))))
