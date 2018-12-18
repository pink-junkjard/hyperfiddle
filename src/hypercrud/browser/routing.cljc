(ns hypercrud.browser.routing                               ; Just merge into context, this is context
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.datomic]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [taoensso.timbre :as timbre]))


(defn route+ "Performs tempid reversal for views downtree"
  [ctx [_ params :as route]]                                ; circular, this can be done sooner
  (mlet [_ (if (or (nil? params) (vector? params))          ; validate normalized already
             (either/right nil)
             (either/left (ex-info "Route not normalized, params must be nil or vector" {:params params})))
         ; route should be a ref, provided by the caller, that we fmap over
         ; because it is not, this is obviously fragile and will break on any change to the route
         ; this is acceptable today (Sep-2018) because changing a route in ANY way assumes the entire iframe will be re-rendered
         reactive-route @(r/apply-inner-r (r/track context/tempid->id+ route ctx))]
    (cats/return (assoc ctx :hypercrud.browser/route reactive-route))))

(defn validated-route+ [?fiddle ?route ctx]                 ; can validate with link, not just fiddle
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [[_ [$1 :as params]] ?route]
    (case (:fiddle/type ?fiddle)
      :query (let [q (unwrap                                ; todo whats the point of this monad?
                       #(timbre/warn %)
                       (mlet [q (reader/memoized-read-string+ (:fiddle/query ?fiddle))]
                         (if (vector? q)
                           (cats/return q)
                           (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))]
               (mlet [q-params (context/validate-query-params+ q params ctx)]
                 ; Ignore the params, just care that they validated.
                 (right ?route)))
      :entity (if (not= nil $1)                             ; todo check conn
                (right ?route)
                (left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right ?route))))
