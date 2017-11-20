(ns hypercrud.browser.base
  (:require [cats.core :as cats :refer [mlet]]
            [cats.monad.either :as either :refer-macros [try-either]]
            [hypercrud.browser.auto-anchor :as auto-anchor]
            [hypercrud.browser.find-element :as find-element]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.q-util :as q-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.browser.user-bindings :as user-bindings]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :as eval]
            [hypercrud.types.Entity :refer [Entity]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest]]
            [hypercrud.types.ThinEntity :refer [ThinEntity]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest]]
            [hypercrud.util.string :as hc-string]))


(defn build-pathed-anchors-lookup [anchors]
  (reduce (fn [acc anchor]
            (-> (hc-string/memoized-safe-read-edn-string (str "[" (:link/path anchor) "]"))
                (either/branch
                  (fn [e]
                    (js/console.error (pr-str e))
                    ; swallow the error
                    acc)
                  (fn [path]
                    (update-in acc (conj path :links) conj anchor)))))
          {}
          anchors))

(def meta-pull-exp-for-link
  ['*
   :db/doc
   :link-query/value
   :request/type
   :fiddle/request
   :fiddle/pull
   {:link-query/find-element [{:find-element/form [{:form/field [:field/attribute :db/doc]}]}]
    :link/anchor ['*
                  ; hydrate the whole link for validating the anchor by query params
                  {:anchor/link ['*]}]}])

(defn meta-request-for-link [ctx]
  (try-either
    (let [link-id (get-in ctx [:route :link-id])
          _ (assert link-id "missing link-id")
          dbval (hc/db (:peer ctx) (get-in ctx [:repository :dbhole/uri]) (:branch ctx))]
      (->EntityRequest link-id nil dbval meta-pull-exp-for-link))))

(defn hydrate-link [ctx]
  (if (auto-link/system-link? (get-in ctx [:route :link-id]))
    {:meta-link-req' (either/right nil)
     :link' (auto-link/hydrate-system-link (get-in ctx [:route :link-id]) ctx)}
    (let [meta-link-request (meta-request-for-link ctx)]
      {:meta-link-req' meta-link-request
       :link' (cats/bind meta-link-request #(hc/hydrate (:peer ctx) %))})))

(defn request-for-link [link ctx]
  (case (:request/type link)
    :query
    (mlet [q (hc-string/memoized-safe-read-edn-string (:link-query/value link))
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
            #_(-> (hc-string/memoized-safe-read-edn-string (:fiddle/pull link))
                  (either/branch (constantly nil) identity))
            missing (->> params (filter (comp nil? second)) (mapv first))]
        (if (empty? missing)
          (cats/return (->QueryRequest q params nil))
          (either/left {:message "missing param" :data {:params params :missing missing}}))))

    :entity
    (let [request-params (get-in ctx [:route :request-params])
          e (:entity request-params)
          uri (try (let [dbname (.-dbname e)]               ;todo report this exception better
                     (get-in ctx [:repository :repository/environment dbname]))
                   (catch :default e nil))
          pull-exp (or (-> (hc-string/memoized-safe-read-edn-string (:fiddle/pull link))
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
                               (eval/eval-str' (from-link link))))
                         (cats/fmap with-user-fn))
                (either/right default))
      :xray (either/right default))))

(let [never-read-only (constantly false)]
  (defn process-results [fiddle request ctx]
    (mlet [schemas (schema-util/hydrate-schema ctx)         ; schema is allowed to be nil if the link only has anchors and no data dependencies
           result (->> (if request
                         (hc/hydrate (:peer ctx) request)
                         (either/right nil)))
           :let [ctx (assoc ctx                             ; provide defaults before user-bindings run.
                       :request request
                       :schemas schemas                     ; For tx/entity->statements in userland.
                       :fiddle fiddle                       ; for :db/doc
                       :read-only (or (:read-only ctx) never-read-only))]
           ctx (user-bindings/user-bindings' fiddle ctx)
           ; todo why are we imposing these auto-fns on everyone?
           :let [ordered-fes (find-element/auto-find-elements result ctx)
                 anchors (auto-anchor/auto-anchors ordered-fes ctx)]]
      (cats/return {:result result
                    :ordered-fes ordered-fes
                    :anchors anchors
                    :ctx ctx}))))

(defn data-from-route [route ctx]
  (let [ctx (context/route ctx route)
        {:keys [link']} (hydrate-link ctx)]
    (mlet [link link'
           link-request (request-for-link link ctx)]
      (process-results link link-request ctx))))

(defn from-anchor [anchor ctx with-route]
  (mlet [route (routing/build-route' anchor ctx)]
    ; entire context must be encoded in the route
    (with-route route (context/clean ctx))))

(defn data-from-anchor [anchor ctx]
  (from-anchor anchor ctx data-from-route))
