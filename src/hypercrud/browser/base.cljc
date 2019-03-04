(ns hypercrud.browser.base
  (:require [cats.core :refer [>>= mlet return]]
            [cats.monad.either :as either :refer [left right]]
            [contrib.ct :refer [unwrap]]
            [contrib.data]
            [contrib.reactive :as r]
            [contrib.reader :as reader :refer [memoized-read-edn-string+]]
            [contrib.try$ :refer [try-either]]
            [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.client.core :as hc]
            [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
            [hypercrud.types.DbRef :refer [->DbRef]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.fiddle :as fiddle]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.ui.sort]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
       (hypercrud.types.DbName DbName))))


(defn legacy-fiddle-ident->lookup-ref [fiddle]              ; SHould be an ident but sometimes is a long today
  ; Keywords are not a db/ident, turn it into the fiddle-id lookup ref.
  ; Otherwise, pass it through, its already a lookup-ref or eid or whatever.
  (cond
    (keyword? fiddle) [:fiddle/ident fiddle]
    (uuid? fiddle) [:fiddle/uuid fiddle]
    :else fiddle))

(defn legacy-lookup-ref->fiddle-ident [lookup-ref]
  (if (coll? lookup-ref)
    (case (first lookup-ref)
      :fiddle/ident (second lookup-ref)
      :fiddle/uuid (second lookup-ref)
      lookup-ref)
    lookup-ref))

(defn meta-request-for-fiddle [ctx]
  (if @(r/fmap->> (:hypercrud.browser/route ctx) first (domain/system-fiddle? (runtime/domain (:peer ctx))))
    (either/right nil)
    (try-either
      (let [fiddle @(r/fmap first (:hypercrud.browser/route ctx))
            _ (assert fiddle "missing fiddle-id")
            dbval (->DbRef (domain/fiddle-dbname (runtime/domain (:peer ctx))) (:branch ctx))]
        (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle) dbval fiddle/browser-pull)))))

(defn validate-fiddle [fiddle]
  (if-not (:db/id fiddle)
    (either/left (ex-info (str :hyperfiddle.error/fiddle-not-found)
                          {:ident :hyperfiddle.error/fiddle-not-found
                           :error-msg "Fiddle not found"
                           :human-hint "Did you just edit :fiddle/ident?"}))
    (either/right fiddle)))

(defn hydrate-fiddle [meta-fiddle-request ctx]
  (let [fiddle-ident (first @(:hypercrud.browser/route ctx))]
    (if (domain/system-fiddle? (runtime/domain (:peer ctx)) fiddle-ident)
      (domain/hydrate-system-fiddle (runtime/domain (:peer ctx)) fiddle-ident)
      (>>= @(hc/hydrate (:peer ctx) (:branch ctx) @meta-fiddle-request) validate-fiddle))))

(defn request-for-fiddle [{fiddle :hypercrud.browser/fiddle :as ctx}] ; depends on route
  ; it's a fiddle-ctx now, which has the defaults applied
  (case @(r/cursor fiddle [:fiddle/type])
    :query (mlet [q (reader/memoized-read-string+ @(r/cursor fiddle [:fiddle/query]))
                  args (context/validate-query-params+ q @(r/fmap second (:hypercrud.browser/route ctx)) ctx)]
             (return (->QueryRequest q args)))

    :entity
    (let [[_ args] @(:hypercrud.browser/route ctx)          ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
          [dbname ?e] (if (instance? DbName (first args))
                        [(:dbname (first args)) (second args)]
                        [nil (first args)])]
      (if-let [dbname (or dbname @(r/cursor fiddle [:fiddle/pull-database]))]
        (let [db (->DbRef dbname (:branch ctx))
              pull-exp (or (-> (memoized-read-edn-string+ @(r/cursor fiddle [:fiddle/pull]))
                               (either/branch (constantly nil) identity))
                           ['*])]
          (either/right (->EntityRequest (or (:db/id ?e) ?e) db pull-exp)))
        (either/left (ex-info "Missing :fiddle/pull-database" {:fiddle @(r/cursor fiddle [:fiddle/ident])}))))

    :blank (either/right nil)

    (either/right nil)))

(let [nil-or-hydrate (fn [peer branch request]
                       (if-let [?request @request]
                         @(hc/hydrate peer branch ?request)
                         (either/right nil)))]
  (defn process-results "Initialize ctx internals, but doesn't focus anything into scope.
    Not even the topfiddle"
    [request ctx]
    ; Blow mlet in case of (right _) -> (left _), but don't recompute if (right :a) -> (right :b).
    (mlet [ctx (context/valid+ ctx)
           ; result SHOULD be sorted out of jvm, though isn't yet
           r-result @(r/apply-inner-r (r/track nil-or-hydrate (:peer ctx) (:branch ctx) request))
           :let [#_#_sort-fn (hyperfiddle.ui.sort/sort-fn % sort-col)
                 ctx (hypercrud.browser.context/result ctx r-result)]]
      (return ctx))))

(defn data-from-route "either ctx, ctx-from-route" [route ctx] ; todo rename
  (mlet [ctx (-> (context/clean ctx)
                 (routing/route+ route))
         meta-fiddle-request @(r/apply-inner-r (r/track meta-request-for-fiddle ctx))
         r-fiddle @(r/apply-inner-r (r/track hydrate-fiddle meta-fiddle-request ctx))
         :let [ctx (context/fiddle ctx r-fiddle)]
         fiddle-request @(r/apply-inner-r (r/track request-for-fiddle ctx))]
    ; fiddle request can be nil for no-arg pulls (just draw readonly form)
    (process-results fiddle-request ctx)))

(defn data-from-route! [route ctx]
  (unwrap (constantly nil) (data-from-route route ctx)))

(defn from-link [link ctx with-route]                       ; ctx is for formula and routing (tempids and domain)
  {:post [%]}
  (let [target-a (hyperfiddle.fiddle/read-path (:link/path link))]
    (mlet [ctx (context/refocus+ ctx target-a)
           args (context/build-args+ ctx link)
           ?route (context/build-route+ args ctx link)]
      (with-route ?route ctx))))

(defn data-from-link [link ctx]                             ; todo rename
  {:pre [link ctx]}
  (from-link link ctx data-from-route))

(defn data-from-link! [link ctx]                            ; mapcat, this can return tuple
  (unwrap #(timbre/warn %) #_(constantly nil)
          (data-from-link link ctx)))
