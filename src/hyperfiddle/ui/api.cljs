(ns hyperfiddle.ui.api
  (:require
    [cats.core :as cats]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hyperfiddle.data :as data]
    [taoensso.timbre :as timbre]))


(declare api-data)
(declare with-result)

(defn recurse-from-route [route ctx]
  (->> (base/data-from-route route ctx)
       (cats/fmap api-data)
       ; todo cannot swallow this error
       (unwrap #(timbre/warn %))))

(defn recurse-from-link [ctx link]
  (->> (base/data-from-link link ctx)
       (cats/fmap api-data)
       ; todo cannot swallow this error
       (unwrap #(timbre/warn %))))

(defn body-field [ctx]
  (->> @(r/fmap->> (:hypercrud.browser/fiddle ctx)
                   :fiddle/links
                   (filter (r/partial link/same-path-as? (:hypercrud.browser/path ctx)))
                   (map (r/partial recurse-from-link ctx))
                   (apply merge))
       (into (let [child-fields? (not @(r/fmap-> (:hypercrud.browser/field ctx) ::field/children nil?))]
               (if (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                 (with-result ctx)
                 {})))))

(defn with-result [ctx]
  (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
    :db.cardinality/one (->> (data/form-with-naked-legacy ctx)
                             (map (fn [path]
                                    (if (seq path)          ; scar - guard infinite recursion on [] links
                                      (body-field (context/focus ctx path)))))
                             (apply merge))
    :db.cardinality/many (->> (:hypercrud.browser/data ctx)
                              (r/unsequence (r/partial data/row-keyfn ctx)) ; the request side does NOT need the cursors to be equiv between loops
                              (map (fn [[row k]]
                                     (let [ctx (context/row ctx row)]
                                       (->> (data/form-with-naked-legacy ctx)
                                            (map (fn [path]
                                                   (if (seq path)
                                                     (body-field (context/focus ctx path)))))
                                            (apply merge)))))
                              (apply merge))
    ; blank fiddles
    {}))

(defn api-data [ctx]
  ; at this point we only care about inline links
  ; also no popovers can be opened, so remove managed
  (let [ctx (update ctx :hypercrud.browser/fiddle (partial r/fmap hypercrud.browser.browser-request/filter-inline-links))]
    (merge (with-result ctx)
           @(r/fmap->> (:hypercrud.browser/fiddle ctx)
                       :fiddle/links
                       (filter (r/partial link/same-path-as? (:hypercrud.browser/path ctx)))
                       (map (r/partial recurse-from-link ctx))
                       (apply merge))
           (when @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
             ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
             ; EntityRequest args are too structured.
             (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)]
               (recurse-from-route [inner-fiddle (vec inner-args)] ctx)))
           {@(:hypercrud.browser/route ctx) @(:hypercrud.browser/data ctx)})))
