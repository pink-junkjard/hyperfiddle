(ns hyperfiddle.foundation
  (:require
    [cats.monad.either :as either]
    #?(:cljs [contrib.css :refer [css]])
    [hypercrud.browser.browser-request :refer [request-from-route]]
    [hypercrud.client.core :as hc]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.domain :as domain]
    #?(:cljs [hyperfiddle.ide.staging])
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.domains]
    #?(:cljs [hyperfiddle.ui.iframe :refer [iframe-cmp]])
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn ^:deprecated route-encode [rt route]
  (timbre/warn "foundation/route-encode is deprecated")
  (domain/url-encode (runtime/domain rt) route))

#?(:cljs
   (defn ^:deprecated stateless-login-url [& args]
     (timbre/warn "foundation/stateless-login-url is deprecated")
     (apply hyperfiddle.ide/stateless-login-url args)))

#?(:cljs
   (defn error-cmp [e]
     [:div
      [:h1 "Fatal error"]
      (if (Err/Err? e)
        [:div
         [:h3 (:msg e)]
         (when-let [data (:data e)]
           [:pre data])]
        [:div
         [:fieldset [:legend "(pr-str e)"]
          [:pre (pr-str e)]]
         [:fieldset [:legend "(ex-data e)"]                 ; network error
          [:pre (str (:data e))]]                           ; includes :body key
         [:fieldset [:legend "(.-stack e)"]                 ; network error
          [:pre (.-stack e)]]])]))

#?(:cljs (defn ^:deprecated staging [ctx & [child]] [hyperfiddle.ide.staging/staging ctx child]))

#?(:cljs
   (defn view [ctx]
     (let [project+ (either/right nil)
           #_(->> (project/project-request ctx)
                  (hc/hydrate (:peer ctx) (:branch ctx))
                  deref)]
       (if-let [e (or @(runtime/state (:peer ctx) [::runtime/fatal-error])
                      (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]))
                      (when (either/left? project+) @project+))]
         [:div                                              ; necessary wrapper div, it is the react root
          [error-cmp e]
          [staging ctx]]
         ; Necessary wrapper div, this is returned from react-render
         [:div
          [:style {:dangerouslySetInnerHTML {:__html (:domain/css @project+)}}]
          (either/branch
            (project/eval-domain-code!+ (:domain/code @project+))
            (fn [e] [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]])
            (fn [_] [iframe-cmp ctx {:route @(runtime/state (:peer ctx) [::runtime/partitions nil :route])}]))]))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-ROUTE 2)
(def LEVEL-LOCAL-BASIS 3)
(def LEVEL-SCHEMA 4)
(def LEVEL-HYDRATE-PAGE 5)

; this needs to be a bit smarter; this should be invoked by everyone (all service endpoints, ssr, browser)
; e.g. for service/hydrate-route, we have route, and local-basis, just need to fetch domain & hydrate
; it makes no sense for clients to forward domains along requests (same as global-basis),
; so we need to inject into the domain level and then continue on at the appropriate level.
; could also handle dirty staging areas for browser
(defn bootstrap-data2 [rt init-level load-level route branch initial-global-basis & [dirty-stage?]]
  (if (>= init-level load-level)
    (p/resolved nil)
    (-> (condp = (inc init-level)
          LEVEL-GLOBAL-BASIS (actions/refresh-global-basis rt nil (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-ROUTE (p/do* (runtime/dispatch! rt [:add-partition branch route]))
          LEVEL-LOCAL-BASIS (-> (actions/refresh-partition-basis rt branch (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
                                (p/then #(runtime/dispatch! rt [:hydrate!-start branch])))
          LEVEL-SCHEMA (actions/hydrate-partition-schema rt branch (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          LEVEL-HYDRATE-PAGE (if (or (not= initial-global-basis @(runtime/state rt [::runtime/global-basis])) dirty-stage?)
                               (actions/hydrate-partition rt branch (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
                               (p/resolved (runtime/dispatch! rt [:hydrate!-shorted branch]))))
        (p/then #(bootstrap-data2 rt (inc init-level) load-level route branch initial-global-basis dirty-stage?)))))

(defn bootstrap-data [rt init-level load-level encoded-route initial-global-basis & [dirty-stage?]]
  (-> (p/do* (domain/url-decode (runtime/domain rt) encoded-route))
      (p/then (fn [route]
                (bootstrap-data2 rt init-level load-level route nil initial-global-basis dirty-stage?)))))
