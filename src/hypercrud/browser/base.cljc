(ns hypercrud.browser.base
  (:require [cats.core :refer [mlet return]]
            [cats.monad.either :as either]
            [contrib.reactive :as r]
            [contrib.reader :as reader :refer [memoized-read-edn-string+]]
            [contrib.try$ :refer [try-either]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.link :as link]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hyperfiddle.ide.system-fiddle :as system-fiddle]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hyperfiddle.domain :as domain]
            [hyperfiddle.fiddle :as fiddle]))


(def meta-pull-exp-for-link
  [:db/id
   :db/doc
   :fiddle/css
   :fiddle/ident
   {:fiddle/links [:db/id
                   :link/class
                   {:link/fiddle [:db/id
                                  :fiddle/ident             ; routing
                                  :fiddle/query             ; validation
                                  :fiddle/type              ; validation
                                  ]}
                   :link/formula
                   :link/ident
                   :link/path
                   :link/rel
                   :link/tx-fn]}
   :fiddle/markdown
   :fiddle/pull
   :fiddle/pull-database
   :fiddle/query
   :fiddle/cljs-ns
   :fiddle/renderer
   :fiddle/type
   :fiddle/hydrate-result-as-fiddle
   '*                                                       ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer
   ])

(defn legacy-fiddle-ident->lookup-ref [fiddle]              ; SHould be an ident but sometimes is a long today
  ; Keywords are not a db/ident, turn it into the fiddle-id lookup ref.
  ; Otherwise, pass it through, its already a lookup-ref or eid or whatever.
  (if (keyword? fiddle)
    [:fiddle/ident fiddle]
    fiddle))

(defn meta-request-for-fiddle [ctx]
  (if @(r/fmap-> (:hypercrud.browser/route ctx) first system-fiddle/system-fiddle?)
    (either/right nil)
    (try-either
      (let [fiddle @(r/fmap first (:hypercrud.browser/route ctx))
            _ (assert fiddle "missing fiddle-id")
            _ (assert (:hypercrud.browser/domain ctx) "missing domain")
            dbval (hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/domain :domain/fiddle-database :database/uri]) (:branch ctx))]
        (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle) dbval meta-pull-exp-for-link)))))

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
      (fiddle/fiddle-defaults fiddle route))))

(defn request-for-fiddle [fiddle ctx]                       ; depends on route
  (case @(r/cursor fiddle [:fiddle/type])
    :query (mlet [q (reader/memoized-read-string+ @(r/cursor fiddle [:fiddle/query]))
                  args (q-util/validate-query-params+ q @(r/fmap second (:hypercrud.browser/route ctx)) ctx)]
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
  (defn process-results [fiddle request ctx]                ; todo rename to (context/result)
    (mlet [reactive-schemas @(r/apply-inner-r (schema-util/hydrate-schema ctx))
           reactive-result @(r/apply-inner-r (r/track nil-or-hydrate (:peer ctx) (:branch ctx) request))
           :let [ctx (assoc ctx
                       :hypercrud.browser/data reactive-result
                       :hypercrud.browser/fiddle fiddle     ; for :db/doc
                       :hypercrud.browser/path []
                       ; For tx/entity->statements in userland.
                       :hypercrud.browser/schemas reactive-schemas)]
           reactive-field @(r/apply-inner-r (r/track field/auto-field request ctx))]
      (return (assoc ctx :hypercrud.browser/field reactive-field)))))

(defn data-from-route [route ctx]                           ; todo rename
  (let [ctx (-> (context/clean ctx)
                (routing/route route))]
    (mlet [meta-fiddle-request @(r/apply-inner-r (r/track meta-request-for-fiddle ctx))
           fiddle @(r/apply-inner-r (r/track hydrate-fiddle meta-fiddle-request ctx))
           fiddle-request @(r/apply-inner-r (r/track request-for-fiddle fiddle ctx))]
      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (process-results fiddle fiddle-request ctx))))

(defn from-link [link ctx with-route]                       ; ctx is for formula and routing (tempids and domain)
  (let [ctx (context/refocus ctx (link/read-path (:link/path link)))] ; symmetry with UI - popovers, txfn etc
    (mlet [route (routing/build-route' ctx link)]
      (with-route route ctx))))

(defn data-from-link [link ctx]                             ; todo rename
  (from-link link ctx data-from-route))
