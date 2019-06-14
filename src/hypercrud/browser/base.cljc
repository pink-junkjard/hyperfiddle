(ns hypercrud.browser.base
  (:require [cats.core :refer [>>= mlet return]]
            [cats.monad.either :as either]
            [contrib.reactive :as r]
            [contrib.reader :as reader :refer [memoized-read-edn-string+]]
            [contrib.try$ :refer [try-either]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.routing :as routing]
            [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
            [hypercrud.types.DbRef :refer [->DbRef]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.fiddle :as fiddle]
            [hyperfiddle.runtime :as runtime])
  #?(:clj
     (:import
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
      (>>= @(runtime/hydrate (:peer ctx) (:branch ctx) @meta-fiddle-request) validate-fiddle))))

(defn request-for-fiddle [{fiddle :hypercrud.browser/fiddle :as ctx}] ; depends on route
  ; it's a fiddle-ctx now, which has the defaults applied
  (case @(r/cursor fiddle [:fiddle/type])
    :query (mlet [q (reader/memoized-read-string+ @(r/cursor fiddle [:fiddle/query]))
                  args (context/validate-query-params+ q @(r/fmap second (:hypercrud.browser/route ctx)) ctx)]
             (return (->QueryRequest q args nil)))

    :entity
    (let [[_ args] @(:hypercrud.browser/route ctx)          ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
          [dbname ?e] (if false #_(instance? DbName (first args))
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

(let [nil-or-hydrate (fn [rt branch request]
                       (if-let [?request @request]
                         @(runtime/hydrate rt branch ?request)
                         (either/right nil)))]
  (defn browse-route+ "either ctx, ctx-from-route" [route ctx]
    (mlet [ctx (-> (context/clean ctx)
                   (routing/route+ route))
           meta-fiddle-request @(r/apply-inner-r (r/track meta-request-for-fiddle ctx))
           r-fiddle @(r/apply-inner-r (r/track hydrate-fiddle meta-fiddle-request ctx))
           ctx (context/fiddle+ ctx r-fiddle)
           fiddle-request @(r/apply-inner-r (r/track request-for-fiddle ctx))
           ; result SHOULD be sorted out of jvm, though isn't yet
           r-result @(r/apply-inner-r (r/track nil-or-hydrate (:peer ctx) (:branch ctx) fiddle-request))
           :let [ctx (hypercrud.browser.context/result ctx r-result)]]
      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (return ctx))))

(defn from-link+ [link-ref ctx with-route+]                   ; ctx is for formula and routing (tempids and domain)
  (mlet [ctx (context/refocus-to-link+ ctx link-ref)
         args (context/build-args+ ctx @link-ref)
         route (context/build-route+ args ctx)]
    (with-route+ route ctx)))

(defn browse-link+ [link-ref ctx]
  {:pre [link-ref ctx]}
  (from-link+ link-ref ctx browse-route+))
