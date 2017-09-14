(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
            [cats.monad.exception :as exception]
            [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.auto-link :as auto-link]
            [hypercrud.browser.base :as base]
            [hypercrud.browser.context :as context]
            [hypercrud.client.core :as hc]
            [hypercrud.client.schema :as schema-util]
            [hypercrud.compile.eval :refer [eval-str']]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.platform.safe-render :refer [safe-user-renderer]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.types.EntityRequest :refer [EntityRequest]]
            [hypercrud.types.QueryRequest :refer [QueryRequest]]
            [hypercrud.ui.auto-control :as auto-control]
            [hypercrud.ui.form-util :as form-util]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.util.monad :refer [exception->either]]
            [reagent.core :as r]))


(declare ui')
(declare ui)

(defn with-reprocessed-result [ui-fn result ordered-fes anchors param-ctx]
  (let [anchors (remove :anchor/disabled? anchors)
        anchor-index (->> anchors
                          (filter :anchor/ident)            ; cannot lookup nil idents
                          (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                          (into {}))
        browse (fn [ident param-ctx f & args]
                 [ui (get anchor-index ident) (assoc param-ctx :user-renderer f #_(if f #(apply f %1 %2 %3 %4 args)))])
        anchor (fn [ident param-ctx label]
                 (let [props (-> (anchor/build-anchor-props (get anchor-index ident) param-ctx)
                                 #_(dissoc :style) #_"custom renderers don't want colored links")]
                   [(:navigate-cmp param-ctx) props label]))
        browse' (fn [ident ctx]
                  (ui' (get anchor-index ident) (assoc ctx :user-renderer identity)))
        anchor* (fn [ident ctx] (anchor/build-anchor-props (get anchor-index ident) ctx))
        param-ctx (assoc param-ctx
                    :anchor anchor
                    :browse browse
                    :anchor* anchor*
                    :browse' browse'

                    ; backwards compat
                    :with-inline-result browse
                    :link-fn (fn [ident label param-ctx] (anchor ident param-ctx label)))]
    ; result is relation or set of relations
    (ui-fn result ordered-fes anchors param-ctx)))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (-> (eval-str' (:link/renderer link))
        (either/branch
          (fn [e] (constantly [:pre (pprint-str e)]))
          (fn [user-fn]
            (fn [result ordered-fes anchors param-ctx]
              [safe-user-renderer user-fn result ordered-fes anchors param-ctx]))))))

(defn result-cmp [link pre-binding-ctx result ordered-fes anchors ctx]
  (let [ui-fn (case @(:display-mode pre-binding-ctx)
                ; todo executing this user-renderer is potentially unsafe
                :user (or (:user-renderer pre-binding-ctx) (link-user-fn link) auto-control/result)
                :xray auto-control/result
                :root auto-control/result)]
    (with-reprocessed-result ui-fn result ordered-fes anchors ctx)))

(defn hydrate-link [link-dbid param-ctx]
  (if (auto-link/system-link? link-dbid)
    (let [system-link-idmap (-> link-dbid :id)]
      (->> (auto-link/request-for-system-link system-link-idmap param-ctx)
           (mapv #(if % (hc/hydrate (:peer param-ctx) %) (either/right nil)))
           (cats/sequence)
           (cats/fmap #(auto-link/hydrate-system-link system-link-idmap % param-ctx))))
    (hc/hydrate (:peer param-ctx) (base/meta-request-for-link link-dbid param-ctx))))

(defn ui-from-route' [{query-params :query-params :as route} param-ctx]
  (try
    (let [param-ctx (context/route param-ctx route)]
      (mlet [link (hydrate-link (:link-dbid route) param-ctx) ; always latest
             ordered-fes (form-util/get-ordered-find-elements link query-params param-ctx)
             request (base/request-for-link link query-params ordered-fes param-ctx)
             result (if request (hc/hydrate (:peer param-ctx) request) (either/right nil))
             ; schema is allowed to be nil if the link only has anchors and no data dependencies
             schemas (schema-util/hydrate-schema ordered-fes param-ctx)
             :let [f (r/partial result-cmp link param-ctx)]]
        (base/process-results f query-params link request result schemas ordered-fes param-ctx)))
    ; js errors? Why do we need this?
    ; user-renderers can throw, should be caught lower though
    (catch :default e (either/left e))))

(defn ui-from-props' [anchor anchor-props ctx]
  (try
    ; if a user is invoking this fn explicitly they probably dont care if the anchor is hidden
    ; todo should filter hidden anchors out before recursing (in widget/render-inline-anchors)
    (if (:hidden anchor-props)
      (either/right [:noscript])                            ; todo cannot return hiccup here, this is a value function
      (mlet [route (anchor/build-anchor-route' anchor ctx)]
        ; entire context must be encoded in the route
        (ui-from-route' route (context/clean ctx))))
    ; js errors? Why do we need this.
    (catch :default e (either/left e))))

(defn ui' [anchor ctx]
  ; js errors? Why do we need this exception monad.
  (-> (exception/try-on (anchor/build-anchor-props anchor ctx)) ; LOOOOOLLLLLL we are dumb
      (exception->either)
      (cats/bind #(ui-from-props' anchor % ctx))))

(defn ui-error-inline [e ctx]
  (let [dev-open? @(r/cursor (-> ctx :peer .-state-atom) [:dev-open])
        detail (if dev-open? (str " -- " (pr-str (:data e))))]
    [:code (:message e) " " detail]))

(defn ui-error-block [e ctx]
  #_(ex-message e) #_(pr-str (ex-data e))
  (let [dev-open? @(r/cursor (-> ctx :peer .-state-atom) [:dev-open])
        detail (if dev-open? (pr-str (:data e)))]
    ; todo we don't always return an error with a message
    [:pre (:message e) "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :entity :attribute :value
  (let [C (cond
            (:ui-error ctx) (:ui-error ctx)                 ; botnav
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn wrap-ui [v' route ctx]
  (let [c #(when (and route (contains? @(r/cursor (-> ctx :peer .-state-atom) [:pressed-keys]) "alt"))
             ((:dispatch! ctx) (actions/set-route route))
             (.stopPropagation %))]
    [native-listener {:on-click c}
     [:div.ui
      (either/branch v' (fn [e] (ui-error e ctx)) identity)]]))

(defn ui-from-route [route ctx]
  [wrap-ui (ui-from-route' route ctx) route ctx])

(defn ui [anchor ctx]
  ; js errors? Why do we need this exception monad.
  (let [anchor-props (-> (exception/try-on (anchor/build-anchor-props anchor ctx)) ; LOOOOOLLLLLL we are dumb
                         (exception->either))
        v' (mlet [anchor-props anchor-props]
             (ui-from-props' anchor anchor-props ctx))
        route (-> (cats/fmap :route anchor-props)
                  (cats/mplus (either/right nil))
                  (cats/extract))]
    [wrap-ui v' route ctx]))
