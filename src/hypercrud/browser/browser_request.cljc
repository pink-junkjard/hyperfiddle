(ns hypercrud.browser.browser-request
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.routing :as routing]
    [hypercrud.client.peer :refer [-quiet-unwrap]]
    [hyperfiddle.api]
    [hyperfiddle.data :as data]
    [hyperfiddle.project :as project]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


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
  (->> @(data/select-many-here ctx)
       (mapcat #(request-from-link % ctx))
       (concat (let [child-fields? (not @(r/fmap (r/comp nil? ::field/children) (:hypercrud.browser/field ctx)))]
                 (when (and child-fields? (context/attribute-segment? (last (:hypercrud.browser/path ctx)))) ; ignore relation and fe fields
                   (with-result ctx))))))

(defn with-result [ctx]
  (for [ctx (hyperfiddle.api/spread-fiddle ctx)
        ctx (hyperfiddle.api/spread-rows ctx)
        ctx (hyperfiddle.api/spread-elements ctx)]
    (->> (data/form-with-naked-legacy ctx)
         ; left the map-flatten but why not mapcat?
         (map (fn [path]
                (if (seq path)                              ; scar - guard infinite recursion on [] links
                  (body-field (context/focus ctx path)))))
         flatten)))

(defn requests [ctx]
  ; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
  (concat
    ; Top level iframes have no dependency at all, no qfind or element at all
    ; Is the topfiddle-ident in scope though?
    (->> @(data/select-many-here ctx #{:hf/iframe nil}) ; No dependency at all, not even topfiddle
         (mapcat #(request-from-link % ctx)))
    (with-result ctx)
    ; This does not get to look at the fiddlescope, though seems reasonable if it wanted to
    (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
      ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
      ; EntityRequest args are too structured.
      (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)]
        (request-from-route [inner-fiddle (vec inner-args)] ctx)))))
