(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet return]]
            [cats.monad.either :as either]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.auto-fiddle :as auto-fiddle]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.find-element :as find-element]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]]
            [hypercrud.util.non-fatal :refer [try-either]]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :as hc-string])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(def meta-pull-exp-for-link
  [:db/id
   :db/doc
   :fiddle/bindings
   :fiddle/entrypoint
   {:fiddle/links [:db/id
                   :hypercrud/props
                   :anchor/prompt
                   :link/create?
                   :link/dependent?
                   :link/disabled?
                   ; hydrate the parts of the fiddle we need for validating the link
                   {:link/fiddle [:db/id
                                  :fiddle/query
                                  :fiddle/type]}
                   :link/formula
                   :link/ident
                   :link/managed?
                   :link/path
                   :link/rel
                   :link/render-inline?
                   :link/tx-fn]}
   :fiddle/name
   :fiddle/pull
   :fiddle/query
   :fiddle/renderer
   :fiddle/request
   :fiddle/type])

(defn meta-request-for-fiddle [ctx]
  (try-either
    (let [fiddle-id (get-in ctx [:route :fiddle-id])
          _ (assert fiddle-id "missing fiddle-id")
          dbval (hc/db (:peer ctx) (get-in ctx [:hypercrud.browser/repository :dbhole/uri]) (:branch ctx))]
      (->EntityRequest fiddle-id nil dbval meta-pull-exp-for-link))))

(defn hydrate-fiddle [ctx]
  {:pre [(-> ctx :hypercrud.browser/repository)
         (-> ctx :hypercrud.browser/repository :dbhole/uri)]}
  (if (auto-fiddle/system-fiddle? (get-in ctx [:route :fiddle-id]))
    {:meta-fiddle-req' (either/right nil)
     :fiddle' (auto-fiddle/hydrate-system-fiddle (get-in ctx [:route :fiddle-id]))}
    (let [meta-fiddle-request (meta-request-for-fiddle ctx)]
      {:meta-fiddle-req' meta-fiddle-request
       :fiddle' (cats/bind meta-fiddle-request #(deref (hc/hydrate (:peer ctx) (:branch ctx) %)))})))

(defn- fix-param [param]
  (cond
    (instance? Entity param) (:db/id param)
    (instance? ThinEntity param) (:db/id param)
    :else param))

(defn validate-query-params [q params ctx]
  {:pre [(vector? q)
         #_(= 0 (count (filter nil? params)))
         #_(do (timbre/debug params q) true)]
   :post [#_(do (timbre/debug %) true)]}
  (mlet [query-holes (try-either (q-util/parse-holes q)) #_"normalizes for :in $"
         :let [;_ (timbre/debug params query-holes q)
               db-lookup (q-util/build-dbhole-lookup ctx)
               ; Add in named database params that aren't formula params
               [params' unused] (loop [acc []
                                       params params
                                       [x & xs] query-holes]
                                  (let [is-db (.startsWith x "$")
                                        next-param (if is-db (get db-lookup x)
                                                             (fix-param (first params)))
                                        rest-params (if is-db params
                                                              (rest params))
                                        acc (conj acc next-param)]
                                    (if xs
                                      (recur acc rest-params xs)
                                      [acc rest-params])))]]
    ;(assert (= 0 (count (filter nil? params')))) ; datomic will give a data source error
    ; validation. better to show the query and overlay the params or something?
    (cond (seq unused) (either/left {:message "unused param" :data {:query q :params params' :unused unused}})
          (not= (count params') (count query-holes)) (either/left {:message "missing params" :data {:query q :params params' :unused unused}})
          :else-valid (either/right params'))))

(defn request-for-fiddle [fiddle ctx]
  (case (:fiddle/type fiddle)
    :query (mlet [q (hc-string/memoized-safe-read-edn-string (:fiddle/query fiddle))
                  params (validate-query-params q (get-in ctx [:route :request-params]) ctx)]
             (return (->QueryRequest q params)))

    :entity
    (let [[e #_"fat" a :as params] (get-in ctx [:route :request-params])
          uri (try (let [dbname (.-dbname e)]               ;todo report this exception better
                     (get-in ctx [:hypercrud.browser/repository :repository/environment dbname]))
                   (catch #?(:clj Exception :cljs js/Error) e nil))
          pull-exp (or (-> (hc-string/memoized-safe-read-edn-string (:fiddle/pull fiddle))
                           (either/branch (constantly nil) identity)
                           first)
                       ['*])]
      (if (or (nil? uri) (nil? (:db/id e)))
        (either/left {:message "malformed entity param" :data {:params params}})
        (either/right
          (->EntityRequest
            (:db/id e) a (hc/db (:peer ctx) uri (:branch ctx)) pull-exp))))

    :blank (either/right nil)

    (either/right nil)))

(defn fn-from-mode [f-mode-config link ctx]
  (let [{:keys [from-ctx from-link with-user-fn default]} f-mode-config]
    (case @(:hypercrud.ui/display-mode ctx)
      :user (->> (or (some-> (from-ctx ctx) either/right)
                     (eval/eval-str (from-link link)))
                 (cats/fmap #(if % (with-user-fn %) default)))
      :xray (either/right default))))

(let [never-read-only (constantly false)
      nil-or-hydrate (fn [peer branch request]
                       (if request
                         @(hc/hydrate peer branch request)
                         (either/right nil)))
      fmap-nil #(cats/fmap (constantly nil) %)]
  (defn process-results [fiddle request ctx]
    (mlet [schemas (schema-util/hydrate-schema ctx)         ; schema is allowed to be nil if the link only has anchors and no data dependencies
           :let [reactive-either-result (reactive/track nil-or-hydrate (:peer ctx) (:branch ctx) request)]
           _ @(reactive/fmap fmap-nil reactive-either-result) ; short the monad, only react on left v right, not the right's value
           :let [reactive-result (reactive/fmap deref reactive-either-result)
                 ctx (assoc ctx                             ; provide defaults before user-bindings run.
                       :hypercrud.browser/result reactive-result
                       :result reactive-result              ; deprecated
                       :request request
                       :schemas schemas                     ; For tx/entity->statements in userland.
                       :fiddle fiddle                       ; for :db/doc
                       :read-only (or (:read-only ctx) never-read-only))]
           ctx (user-bindings/user-bindings' fiddle ctx)
           :let [reactive-either-fes (reactive/track find-element/auto-find-elements reactive-result fiddle request (:route ctx) schemas)]
           _ @(reactive/fmap fmap-nil reactive-either-fes)  ; short the monad, only react on left v right, not the right's value
           :let [reactive-fes (reactive/fmap deref reactive-either-fes)]]
      (cats/return
        (assoc ctx
          :hypercrud.browser/ordered-fes reactive-fes
          :hypercrud.browser/links (reactive/track auto-anchor/auto-links fiddle reactive-fes schemas (:keep-disabled-anchors? ctx)))))))

(defn data-from-route [route ctx]                           ; todo rename
  (let [ctx (context/route ctx route)
        {:keys [fiddle']} (hydrate-fiddle ctx)]
    (mlet [fiddle fiddle'
           fiddle-request (request-for-fiddle fiddle ctx)]
      (process-results fiddle fiddle-request ctx))))

(defn from-link [link ctx with-route]
  (mlet [route (routing/build-route' link ctx)]
    ; entire context must be encoded in the route
    (with-route route (context/clean ctx))))

(defn data-from-link [link ctx]                             ; todo rename
  (from-link link ctx data-from-route))
