(ns hypercrud.browser.browser-ui
  (:require [cats.core :as cats :refer-macros [mlet]]
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
            [reagent.core :as r]))

(defn hydrate-link [link-dbid param-ctx]
  (if (auto-link/system-link? link-dbid)
    (let [system-link-idmap (-> link-dbid :id)]
      (->> (auto-link/request-for-system-link system-link-idmap param-ctx)
           (mapv #(if % (hc/hydrate (:peer param-ctx) %) (either/right nil)))
           (cats/sequence)
           (cats/fmap #(auto-link/hydrate-system-link system-link-idmap % param-ctx))))
    (hc/hydrate (:peer param-ctx) (base/meta-request-for-link link-dbid param-ctx))))

(declare user-result)

(defn get-ui-f [link param-ctx]
  (let [f (case (:display-mode param-ctx)
            :user (user-result link param-ctx)
            :xray auto-control/result
            :root auto-control/result)]
    (fn [relations colspec anchors ctx]
      (let [anchors (remove :anchor/disabled? anchors)]
        (f relations colspec anchors ctx)))))

(defn ui' [{query-params :query-params :as route} param-ctx
           ; hack for first pass on select options
           & [override-get-ui-f]]
  (let [param-ctx (context/route param-ctx route)
        get-ui-f (or override-get-ui-f get-ui-f)]
    (mlet [link (hydrate-link (:link-dbid route) param-ctx) ; always latest
           ordered-fes (form-util/get-ordered-find-elements-m link query-params param-ctx)
           request (base/request-for-link link query-params ordered-fes param-ctx)
           result (if request (hc/hydrate (:peer param-ctx) request) (either/right nil))
           ; schema is allowed to be nil if the link only has anchors and no data dependencies
           schemas (schema-util/hydrate-schema ordered-fes param-ctx)]
      (base/process-results get-ui-f query-params link request result schemas ordered-fes param-ctx))))

(defn ui [anchor param-ctx]
  (let [anchor-props (anchor/build-anchor-props anchor param-ctx)] ; LOOOOOLLLLLL we are dumb
    (if (:hidden anchor-props)
      (either/right [:noscript])
      (mlet [route (anchor/build-anchor-route' anchor param-ctx)]
        (ui' route
             ; entire context must be encoded in the route
             (context/clean param-ctx))))))

(defn ui-error-inline [e ctx]
  (let [detail (if (:dev-open ctx) (str " -- " (pr-str (:data e))))]
    [:code.ui (:message e) " " detail]))

(defn ui-error-block [e ctx]
  #_(ex-message e) #_(pr-str (ex-data e))
  (let [detail (if (:dev-open ctx) (pr-str (:data e)))]
    [:pre.ui (:message e) "\n" detail]))

(defn ui-error [e ctx]
  ; :find-element :entity :attribute :value
  (let [C (cond
            (:ui-error ctx) (:ui-error ctx)                 ; botnav
            (:attribute ctx) ui-error-inline                ; table: header or cell, form: header or cell
            (:find-element ctx) ui-error-inline             ;
            :else ui-error-block)]                          ; browser including inline true links
    [C e ctx]))

(defn safe-ui' [route ctx]
  (either/branch
    (try (ui' route ctx) (catch :default e (either/left e))) ; js errors? Why do we need this.
    (fn [e] (ui-error e ctx))                               ; @(r/cursor (-> ctx :peer .-state-atom) [:pressed-keys])
    (fn [v] (let [c #(if (contains? @(r/cursor (-> ctx :peer .-state-atom) [:pressed-keys]) "alt")
                       (do ((:dispatch! ctx) (actions/set-route route)) (.stopPropagation %)))]
              [native-listener {:on-click c} [:div.ui v]]))))

(defn safe-ui [anchor ctx]
  (either/branch
    (try (ui anchor ctx) (catch :default e (either/left e))) ; js errors? Why do we need this.
    (fn [e] (ui-error e ctx))
    (fn [v] (let [{route :route} (anchor/build-anchor-props anchor ctx)
                  c #(if (contains? @(r/cursor (-> ctx :peer .-state-atom) [:pressed-keys]) "alt")
                       (do ((:dispatch! ctx) (actions/set-route route)) (.stopPropagation %)))]
              [native-listener {:on-click c} [:div.ui v]]))))

(defn link-user-fn [link]
  (if-not (empty? (:link/renderer link))
    (-> (eval-str' (:link/renderer link))
        (either/branch
          (fn [e] (constantly [:pre (pprint-str e)]))
          (fn [user-fn]
            (fn [result colspec anchors param-ctx] [safe-user-renderer user-fn result colspec anchors param-ctx]))))))

(defn user-result [link param-ctx]
  ; only need a safewrap on other people's user-fns; this context's user fn only needs the topmost safewrap.
  (let [user-fn (or (:user-renderer param-ctx) (link-user-fn link) auto-control/result)]
    (fn [result colspec anchors param-ctx]
      (let [anchor-index (->> anchors
                              (filter :anchor/ident)        ; cannot lookup nil idents
                              (mapv (juxt #(-> % :anchor/ident) identity)) ; [ repeating entity attr ident ]
                              (into {}))
            browse (fn [ident param-ctx f & args]
                     [safe-ui (get anchor-index ident) (assoc param-ctx :user-renderer f #_(if f #(apply f %1 %2 %3 %4 args)))])
            anchor (fn [ident param-ctx label]
                     (let [props (-> (anchor/build-anchor-props (get anchor-index ident) param-ctx)
                                     #_(dissoc :style) #_"custom renderers don't want colored links")]
                       [(:navigate-cmp param-ctx) props label]))
            browse' (fn [ident ctx]
                      (ui (get anchor-index ident) (assoc param-ctx :user-renderer identity)))
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
        (user-fn result colspec anchors param-ctx)))))