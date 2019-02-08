(ns hypercrud.browser.browser-request
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.datomic]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
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
                     #_(context/schemas (r/track context/summon-schemas-grouped-by-dbname ctx))
                     (routing/route+ route)
                     -quiet-unwrap)]
    (when-let [meta-fiddle-request (-quiet-unwrap @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx)))]
      (assert (r/reactive? meta-fiddle-request))
      (concat [@meta-fiddle-request
               (project/attrs-request ctx)]
              (-quiet-unwrap
                (mlet [r-fiddle @(r/apply-inner-r (r/track base/hydrate-fiddle meta-fiddle-request ctx))
                       :let [ctx (context/schemas ctx (r/track context/summon-schemas-grouped-by-dbname ctx))
                             ctx (context/fiddle ctx r-fiddle)]
                       fiddle-request @(r/apply-inner-r (r/track base/request-for-fiddle ctx))]
                  (s/assert r/reactive? r-fiddle)
                  (s/assert r/reactive? fiddle-request)
                  (cats/return
                    (concat
                      (some-> @fiddle-request vector)
                      (->> (base/process-results fiddle-request ctx)
                           ; Don't set context/fiddle (would set A to fiddle-ident)
                           ; Context methods must be robust to fiddle-attrs now.
                           (cats/fmap requests)
                           (-quiet-unwrap))))))))))

(defn request-from-link [link ctx]
  ; can return two links worth of requests, but the mapcat will work
  (-quiet-unwrap (base/from-link link ctx (fn [route ctx]
                                            (either/right (request-from-route route ctx))))))

(defn requests-here [ctx]
  (->> @(data/select-many-here ctx #{:hf/iframe})
       (mapcat #(request-from-link % ctx))))

; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
; On the request side, we walk the whole resultset and load each iframe from exactly the right place
; without any refocusing. Only on the view side do we care about drawing things in some other place.

(defn cross-streams [ctx]
  ; This does not get to look at the fiddlescope, though seems reasonable if it wanted to
  (if @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
    ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
    ; EntityRequest args are too structured.
    (let [[_ [inner-fiddle & inner-args]] @(:hypercrud.browser/route ctx)]
      (request-from-route [inner-fiddle (vec inner-args)] ctx))))

(defn request-attr-level [ctx]
  (for [[a ctx] (hypercrud.browser.context/spread-attributes ctx)]
    [(requests-here ctx)
     ; UnsupportedOperationException: Can only recur from tail position
     (request-attr-level ctx)]))

(defn requests [ctx]
  ; More efficient to drive from links. But to do this, we need to refocus
  ; from the top, multiplying out for all possible dependencies.
  (if (hypercrud.browser.context/valid? ctx)
    (flatten
      [(requests-here ctx)
       (for [[_ ctx] (hypercrud.browser.context/spread-result ctx)]
         [(requests-here ctx)
          (for [[_ ctx] (hypercrud.browser.context/spread-rows ctx)]
            [(requests-here ctx)
             (for [[i ctx] (hypercrud.browser.context/spread-elements ctx)]
               [(requests-here ctx)
                (request-attr-level ctx)])])])
       (cross-streams ctx)])))