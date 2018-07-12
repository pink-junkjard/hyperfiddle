(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet return]]
            [cats.monad.either :as either]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.try :refer [try-either]]
            [hypercrud.browser.auto-link :refer [auto-links]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.fiddle :as fiddle]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(def meta-pull-exp-for-link
  [:db/id
   :db/doc
   :fiddle/bindings
   :fiddle/css
   :fiddle/ident
   {:fiddle/links [:db/id
                   :hypercrud/props
                   :link/create?
                   :link/disabled?
                   {:link/fiddle [:db/id
                                  :fiddle/ident             ; routing
                                  :fiddle/query             ; validation
                                  :fiddle/type              ; validation
                                  ]}
                   :link/formula
                   :link/ident
                   :link/managed?
                   :link/path
                   :link/rel
                   :link/render-inline?
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
  (if (system-fiddle/system-fiddle? (get-in ctx [:route 0]))
    (either/right nil)
    (try-either
      (let [[fiddle] (get-in ctx [:route])
            _ (assert fiddle "missing fiddle-id")
            _ (assert (:hypercrud.browser/domain ctx) "missing domain")
            dbval (hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/domain :domain/fiddle-repo]) (:branch ctx))]
        (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle) nil dbval meta-pull-exp-for-link)))))

(defn validate-fiddle [fiddle]
  (if-not (:db/id fiddle)
    (either/left (ex-info (str :hyperfiddle.error/fiddle-not-found)
                          {:ident :hyperfiddle.error/fiddle-not-found
                           :error-msg "Fiddle not found"
                           :human-hint "Did you just edit :fiddle/ident?"}))
    (either/right fiddle)))

(defn hydrate-fiddle [meta-fiddle-request ctx]
  (let [auto-fiddle (system-fiddle/system-fiddle? (get-in ctx [:route 0]))]
    (mlet [fiddle (if auto-fiddle
                    (system-fiddle/hydrate-system-fiddle (get-in ctx [:route 0]))
                    (mlet [fiddle @(hc/hydrate (:peer ctx) (:branch ctx) @meta-fiddle-request)]
                      (validate-fiddle (into {} fiddle))))]
      (return
        (fiddle/fiddle-defaults fiddle)))))

(defn- fix-param [param]
  (cond
    (instance? Entity param) (:db/id param)
    (instance? ThinEntity param) (:db/id param)
    :else param))

(defn validate-query-params [q args ctx]
  (mlet [query-holes (try-either (q-util/parse-holes q)) #_"normalizes for :in $"
         :let [;_ (timbre/debug params query-holes q)
               db-lookup (q-util/build-dbhole-lookup ctx)
               ; Add in named database params that aren't formula params
               [params' unused] (loop [acc []
                                       args args
                                       [x & xs] query-holes]
                                  (let [is-db (.startsWith x "$")
                                        next-arg (if is-db (get db-lookup x)
                                                           (fix-param (first args)))
                                        args (if is-db args (rest args))
                                        acc (conj acc next-arg)]
                                    (if xs
                                      (recur acc args xs)
                                      [acc args])))]]
    ;(assert (= 0 (count (filter nil? params')))) ; datomic will give a data source error
    ; validation. better to show the query and overlay the params or something?
    (cond #_#_(seq unused) (either/left {:message "unused param" :data {:query q :params params' :unused unused}})
      (not= (count params') (count query-holes)) (either/left {:message "missing params" :data {:query q :params params' :unused unused}})
      :else-valid (either/right params'))))

(defn request-for-fiddle [fiddle ctx]                       ; depends on route
  (case @(r/cursor fiddle [:fiddle/type])
    :query (mlet [q (memoized-safe-read-edn-string @(r/cursor fiddle [:fiddle/query]))
                  args (validate-query-params q (get-in ctx [:route 1]) ctx)]
             (return (->QueryRequest q args)))

    :entity
    (if-let [dbname @(r/cursor fiddle [:fiddle/pull-database])]
      (if-let [uri (get-in ctx [:hypercrud.browser/domain :domain/environment dbname])]
        (let [[_ [?e a :as args]] (get-in ctx [:route])     ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
              db (hc/db (:peer ctx) uri (:branch ctx))
              pull-exp (or (-> (memoized-safe-read-edn-string @(r/cursor fiddle [:fiddle/pull]))
                               (either/branch (constantly nil) identity))
                           ['*])]
          (either/right (->EntityRequest (or (:db/id ?e) ?e) a db pull-exp)))
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
                       :hypercrud.browser/path []
                       :hypercrud.browser/fiddle fiddle     ; for :db/doc
                       :hypercrud.browser/request request
                       :hypercrud.browser/result reactive-result
                       ; For tx/entity->statements in userland.
                       :hypercrud.browser/schemas reactive-schemas)]
           ctx (user-bindings/user-bindings ctx)
           reactive-fields @(r/apply-inner-r (r/track field/auto-fields ctx))
           :let [ctx (assoc ctx :hypercrud.browser/fields reactive-fields)
                 ctx (assoc ctx :hypercrud.browser/links (r/track auto-links ctx))
                 ctx (if (and (= :entity @(r/cursor fiddle [:fiddle/type]))
                              ; e is nil on the EntityRequest
                              (-> ctx :route second first nil?))
                       (assoc ctx :read-only (r/constantly true))
                       ctx)
                 ctx (context/focus ctx [:body])           ; todo remove, call (focus ctx [:body]) when you need the data
                 ]]
      (cats/return ctx))))

(defn data-from-route [route ctx]                           ; todo rename
  (let [ctx (-> (context/clean ctx)
                (routing/route route))]
    (mlet [meta-fiddle-request @(r/apply-inner-r (r/track meta-request-for-fiddle ctx))
           fiddle @(r/apply-inner-r (r/track hydrate-fiddle meta-fiddle-request ctx))
           fiddle-request @(r/apply-inner-r (r/track request-for-fiddle fiddle ctx))]
      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (process-results fiddle fiddle-request ctx))))

(defn from-link [link ctx with-route]
  (mlet [route (routing/build-route' link ctx)]
    ; entire context must be encoded in the route
    (with-route route ctx)))

(defn data-from-link [link ctx]                             ; todo rename
  (from-link link ctx data-from-route))
