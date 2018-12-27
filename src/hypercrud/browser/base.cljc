(ns hypercrud.browser.base
  (:require [cats.core :refer [mlet return]]
            [cats.monad.either :as either :refer [left right]]
            [contrib.ct :refer [unwrap]]
            [contrib.reactive :as r]
            [contrib.reader :as reader :refer [memoized-read-edn-string+]]
            [contrib.try$ :refer [try-either]]
            [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.routing :as routing]
            [hyperfiddle.ide.system-fiddle :as system-fiddle]
            [hypercrud.client.core :as hc]
            [hyperfiddle.project :as project]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.fiddle :as fiddle])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(defn legacy-fiddle-ident->lookup-ref [fiddle]              ; SHould be an ident but sometimes is a long today
  ; Keywords are not a db/ident, turn it into the fiddle-id lookup ref.
  ; Otherwise, pass it through, its already a lookup-ref or eid or whatever.
  (cond
    (keyword? fiddle) [:fiddle/ident fiddle]
    (uuid? fiddle) [:fiddle/uuid fiddle]
    :else fiddle))

(defn meta-request-for-fiddle [ctx]
  (if @(r/fmap-> (:hypercrud.browser/route ctx) first system-fiddle/system-fiddle?)
    (either/right nil)
    (try-either
      (let [fiddle @(r/fmap first (:hypercrud.browser/route ctx))
            _ (assert fiddle "missing fiddle-id")
            _ (assert (:hypercrud.browser/domain ctx) "missing domain")
            dbval (hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/domain :domain/fiddle-database :database/uri]) (:branch ctx))]
        (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle) dbval fiddle/browser-pull)))))

(defn validate-fiddle [fiddle]
  (if-not (:db/id fiddle)
    (either/left (ex-info (str :hyperfiddle.error/fiddle-not-found)
                          {:ident :hyperfiddle.error/fiddle-not-found
                           :error-msg "Fiddle not found"
                           :human-hint "Did you just edit :fiddle/ident?"}))
    (either/right fiddle)))

(defn hydrate-fiddle [meta-fiddle-request ctx]
  (mlet [:let [[arg1 :as route] @(:hypercrud.browser/route ctx)]
         fiddle (if (system-fiddle/system-fiddle? arg1)
                  (system-fiddle/hydrate-system-fiddle arg1)
                  (mlet [fiddle @(hc/hydrate (:peer ctx) (:branch ctx) @meta-fiddle-request)]
                    (validate-fiddle fiddle)))]
    (return
      (fiddle/apply-defaults fiddle))))

(defn request-for-fiddle [fiddle ctx]                       ; depends on route
  (case @(r/cursor fiddle [:fiddle/type])
    :query (mlet [q (reader/memoized-read-string+ @(r/cursor fiddle [:fiddle/query]))
                  args (context/validate-query-params+ q @(r/fmap second (:hypercrud.browser/route ctx)) ctx)]
             (return (->QueryRequest q args)))

    :entity
    (if-let [dbname @(r/cursor fiddle [:fiddle/pull-database])]
      (if-let [uri (domain/dbname->uri dbname (:hypercrud.browser/domain ctx))]
        (let [[_ [?e :as args]] @(:hypercrud.browser/route ctx) ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
              db (hc/db (:peer ctx) uri (:branch ctx))
              pull-exp (or (-> (memoized-read-edn-string+ @(r/cursor fiddle [:fiddle/pull]))
                               (either/branch (constantly nil) identity))
                           ['*])]
          (either/right (->EntityRequest (or (:db/id ?e) ?e) db pull-exp)))
        (either/left (ex-info (str "Invalid :fiddle/pull-database " dbname) {})))
      (either/left (ex-info "Missing :fiddle/pull-database" {:fiddle @(r/cursor fiddle [:fiddle/ident])})))

    :blank (either/right nil)

    (either/right nil)))

(let [nil-or-hydrate (fn [peer branch request]
                       (if-let [?request @request]
                         @(hc/hydrate peer branch ?request)
                         (either/right nil)))]
  (defn process-results "Initialize ctx internals, but doesn't focus anything into scope.
    Not even the topfiddle"
    [fiddle request ctx]
    (mlet [reactive-attrs @(r/apply-inner-r (project/hydrate-attrs ctx))
           reactive-result @(r/apply-inner-r (r/track nil-or-hydrate (:peer ctx) (:branch ctx) request))
           :let [ctx (assoc ctx
                       :hypercrud.browser/attr-renderers reactive-attrs
                       ; The following are internal, result is not formally in user scope yet!
                       :hypercrud.browser/data reactive-result
                       :hypercrud.browser/fiddle fiddle
                       :hypercrud.browser/link-index (context/index-links (context/summon-schemas-grouped-by-dbname ctx) fiddle)
                       :hypercrud.browser/eav (r/track identity [nil nil nil]) ; good enough for a top iframe!
                       :hypercrud.browser/path [])]
           reactive-field @(r/apply-inner-r (r/track field/auto-field request ctx))] ; legacy
      (return (assoc ctx :hypercrud.browser/field reactive-field)))))

(defn data-from-route "either ctx, ctx-from-route" [route ctx]                           ; todo rename
  (mlet [ctx (-> (context/clean ctx)
                 (routing/route+ route))
         meta-fiddle-request @(r/apply-inner-r (r/track meta-request-for-fiddle ctx))
         fiddle @(r/apply-inner-r (r/track hydrate-fiddle meta-fiddle-request ctx))
         fiddle-request @(r/apply-inner-r (r/track request-for-fiddle fiddle ctx))]
    ; fiddle request can be nil for no-arg pulls (just draw readonly form)
    (process-results fiddle fiddle-request ctx)))

(defn data-from-route! [route ctx]
  (unwrap (constantly nil) (data-from-route route ctx)))

(defn from-link [link ctx with-route]                       ; ctx is for formula and routing (tempids and domain)
  #_{:pre [(:hypercrud.browser/element ctx)]}               ; top-level iframes dont have an element
  (let [+args (if-let [target-attr (hyperfiddle.fiddle/read-path (:link/path link))] ; set fiddle-ident as attr if you need the dependency
                (context/build-args+ (context/refocus ctx target-attr) link)
                (right []))]                                ; top iframes without dependency don't need a formula
    ; If we're refocusing to :blank, do we need to whack the ctx?
    (mlet [route (context/build-route' +args ctx link)]
      (with-route route ctx))))

(defn data-from-link [link ctx]                             ; todo rename
  (from-link link ctx data-from-route))

(defn data-from-link! [link ctx]
  (unwrap (constantly nil) (data-from-link link ctx)))
