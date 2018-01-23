(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
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
    (let [fiddle-id (get-in ctx [:route :link-id])
          _ (assert fiddle-id "missing link-id")
          dbval (hc/db (:peer ctx) (get-in ctx [:repository :dbhole/uri]) (:branch ctx))]
      (->EntityRequest fiddle-id nil dbval meta-pull-exp-for-link))))

(defn hydrate-fiddle [ctx]
  {:pre [(-> ctx :repository)
         (-> ctx :repository :dbhole/uri)]}
  (if (auto-fiddle/system-fiddle? (get-in ctx [:route :link-id]))
    {:meta-fiddle-req' (either/right nil)
     :fiddle' (auto-fiddle/hydrate-system-fiddle (get-in ctx [:route :link-id]))}
    (let [meta-fiddle-request (meta-request-for-fiddle ctx)]
      {:meta-fiddle-req' meta-fiddle-request
       :fiddle' (cats/bind meta-fiddle-request #(deref (hc/hydrate (:peer ctx) %)))})))

(defn request-for-fiddle [fiddle ctx]
  (case (:fiddle/type fiddle)
    :query
    (mlet [q (hc-string/memoized-safe-read-edn-string (:fiddle/query fiddle))
           query-holes (try-either (q-util/parse-holes q))]
      (let [params-map (merge (get-in ctx [:route :request-params]) (q-util/build-dbhole-lookup ctx))
            params (->> query-holes
                        (mapv (juxt identity (fn [hole-name]
                                               (let [param (get params-map hole-name)]
                                                 (cond
                                                   (instance? Entity param) (:db/id param)
                                                   (instance? ThinEntity param) (:db/id param)
                                                   :else param)))))
                        (into {}))
            ;pull-exp
            #_(-> (hc-string/memoized-safe-read-edn-string (:fiddle/pull fiddle))
                  (either/branch (constantly nil) identity))
            missing (->> params (filter (comp nil? second)) (mapv first))]
        (if (empty? missing)
          (cats/return (->QueryRequest q params))
          (either/left {:message "missing param" :data {:params params :missing missing}}))))

    :entity
    (let [request-params (get-in ctx [:route :request-params])
          e (:entity request-params)
          uri (try (let [dbname (.-dbname e)]               ;todo report this exception better
                     (get-in ctx [:repository :repository/environment dbname]))
                   (catch #?(:clj Exception :cljs js/Error) e nil))
          pull-exp (or (-> (hc-string/memoized-safe-read-edn-string (:fiddle/pull fiddle))
                           (either/branch (constantly nil) identity)
                           first)
                       ['*])]
      (if (or (nil? uri) (nil? (:db/id e)))
        (either/left {:message "missing param" :data {:params request-params
                                                      :missing #{:entity}}})
        (either/right
          (->EntityRequest
            (:db/id e)
            (:a request-params)
            (hc/db (:peer ctx) uri (:branch ctx))
            pull-exp))))

    :blank (either/right nil)

    (either/right nil)))

(defn fn-from-mode [f-mode-config link ctx]
  (let [{:keys [from-ctx from-link with-user-fn default]} f-mode-config]
    (case @(:display-mode ctx)
      ; todo report eval and invocation errors back to the user
      :user (or (some->> (or (some-> (from-ctx ctx) either/right)
                             (if-not (empty? (from-link link))
                               (eval/eval-str (from-link link))))
                         (cats/fmap with-user-fn))
                (either/right default))
      :xray (either/right default))))

(let [never-read-only (constantly false)
      nil-or-hydrate (fn [peer request]
                       (if request
                         @(hc/hydrate peer request)
                         (either/right nil)))]
  (defn process-results [fiddle request ctx]
    (mlet [schemas (schema-util/hydrate-schema ctx)         ; schema is allowed to be nil if the link only has anchors and no data dependencies
           :let [reactive-either-result (reactive/track nil-or-hydrate (:peer ctx) request)]
           _ @reactive-either-result                        ; short the monad
           :let [reactive-result (reactive/map deref reactive-either-result)
                 ctx (assoc ctx                             ; provide defaults before user-bindings run.
                       :request request
                       :schemas schemas                     ; For tx/entity->statements in userland.
                       :fiddle fiddle                       ; for :db/doc
                       :read-only (or (:read-only ctx) never-read-only))]
           ctx (user-bindings/user-bindings' fiddle ctx)
           ; todo why are we imposing these auto-fns on everyone?
           ordered-fes (find-element/auto-find-elements @reactive-result ctx)]
      (cats/return {:result reactive-result
                    :ordered-fes ordered-fes
                    :links (auto-anchor/auto-links ordered-fes ctx)
                    :ctx ctx}))))

(defn data-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [fiddle']} (hydrate-fiddle ctx)]
    (mlet [fiddle fiddle'
           fiddle-request (request-for-fiddle fiddle ctx)]
      (process-results fiddle fiddle-request ctx))))

(defn from-link [link ctx with-route]
  (mlet [route (routing/build-route' link ctx)]
    ; entire context must be encoded in the route
    (with-route route (context/clean ctx))))

(defn data-from-link [link ctx]
  (from-link link ctx data-from-route))
