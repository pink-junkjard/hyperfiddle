(ns hyperfiddle.service.ssr.core
  (:require
    ["react-dom/server" :as dom-server]
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.template :refer [load-resource]]
    [goog.object :as object]
    [hypercrud.browser.context :as context]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hypercrud.transit :as transit]
    [hypercrud.ui.error :as error]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide :as ide]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.state :as state]
    [hyperfiddle.ui.loading :refer [loading-page]]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [reagent.impl.template :as tmpl]
    [reagent.impl.util :as rutil]
    [reagent.ratom :as ratom]
    [taoensso.timbre :as timbre]))


(defn render-to-node-stream
  [component]
  (ratom/flush!)
  (binding [rutil/*non-reactive* true]
    (dom-server/renderToNodeStream (tmpl/as-element component))))

(def analytics (load-resource "analytics.html"))

(defn root-html-str [rt]
  (let [ctx {:peer rt
             ::runtime/branch-aux {::ide/foo "page"}}]
    (try
      (reagent-server/render-to-static-markup
        [foundation/view :page ctx (if (:active-ide? (runtime/host-env rt))
                                     (constantly [loading-page])
                                     ide/view)])
      (catch :default e
        (reagent-server/render-to-string [:div
                                          [:h1 "Javascript mounting..."]
                                          [:h2 "SSR failed on:"]
                                          [error/error-block e]])))))

(defn full-html [build static-resources show-analytics rt client-params]
  (let [hyperfiddle-dns? (boolean (:auth/root (runtime/host-env rt)))
        serve-js? (or (:active-ide? (runtime/host-env rt)) (not @(runtime/state rt [::runtime/domain :domain/disable-javascript])))
        resource-base (str static-resources "/" build)]
    [:html {:lang "en"}
     [:head
      [:title "Hyperfiddle"]
      [:link {:rel "stylesheet" :href (str resource-base "/styles.css")}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:meta {:charset "UTF-8"}]
      [:script {:id "build" :type "text/plain" :dangerouslySetInnerHTML {:__html build}}]]
     [:body
      [:div {:id "root" :dangerouslySetInnerHTML {:__html (root-html-str rt)}}]
      (when (and hyperfiddle-dns? show-analytics)
        [:div {:dangerouslySetInnerHTML {:__html analytics}}])
      (when serve-js?
        ; env vars for client side rendering
        [:script {:id "params" :type "application/transit-json"
                  :dangerouslySetInnerHTML {:__html (transit/encode client-params)}}])
      (when serve-js?
        [:script {:id "state" :type "application/transit-json"
                  :dangerouslySetInnerHTML {:__html (transit/encode @(runtime/state rt))}}])
      (when serve-js?
        [:script {:id "preamble" :src (str resource-base "/preamble.js")}])
      (when serve-js?
        [:script {:id "main" :src (str resource-base "/main.js")}])]]))

(defn ssr [env rt path]
  (let [build (:BUILD env)
        static-resources (:STATIC_RESOURCES env)
        show-analytics (:ANALYTICS env)
        load-level foundation/LEVEL-HYDRATE-PAGE
        browser-init-level (if (:active-ide? (runtime/host-env rt))
                             ;force the browser to re-run the data bootstrapping when not aliased
                             foundation/LEVEL-NONE
                             load-level)]
    (-> (foundation/bootstrap-data rt foundation/LEVEL-NONE load-level path @(runtime/state rt [::runtime/global-basis]))
        (p/then (fn []
                  (->> @(runtime/state rt [::runtime/domain :domain/databases])
                       (map :domain.database/record)
                       (map (juxt :database/uri (fn [hf-db]
                                                  (let [subject @(runtime/state rt [::runtime/user-id])
                                                        user @(runtime/state rt [::runtime/user])]
                                                    (either/branch
                                                      (security/subject-can-transact? hf-db subject user)
                                                      (constantly false)
                                                      identity)))))
                       (into {})
                       (vector :set-auto-transact)
                       (runtime/dispatch! rt))))
        (p/then (constantly 200))
        (p/catch #(or (:hyperfiddle.io/http-status-code (ex-data %)) 500))
        (p/then (fn [http-status-code]
                  (let [client-params {:host-env (runtime/host-env rt)
                                       :sentry {:dsn (:SENTRY_DSN env)
                                                :environment (:SENTRY_ENV env)
                                                :release (:BUILD env)}
                                       :hyperfiddle.bootstrap/init-level browser-init-level}]
                    {:http-status-code http-status-code
                     :component [full-html build static-resources show-analytics rt client-params]}))))))
