(ns hypercrud.browser.routing                               ; todo dead ns
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.reader :as reader]
    [hypercrud.browser.context :as context]
    [taoensso.timbre :as timbre]))


; todo just inline in ui
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
               (mlet [q-params (context/validate-query-params+ (:peer ctx) (:branch ctx) q params)]
                 ; Ignore the params, just care that they validated.
                 (right ?route)))
      :entity (if (not= nil $1)                             ; todo check conn
                (right ?route)
                (left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right ?route))))
