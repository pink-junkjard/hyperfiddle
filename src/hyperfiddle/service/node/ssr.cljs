(ns hyperfiddle.service.node.ssr
  (:require
    [cats.monad.either :as either]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [map-values]]
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
    [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
    [hyperfiddle.io.hydrate-route :refer [hydrate-route-rpc!]]
    [hyperfiddle.io.sync :refer [sync-rpc!]]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.state :as state]
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
    (.renderToNodeStream (reagent-server/module) (tmpl/as-element component))))

(def analytics (load-resource "analytics.html"))

(defn full-html [env state-val serve-js? hyperfiddle-dns? params app-component]
  (let [resource-base (str (:STATIC_RESOURCES env) "/" (:BUILD env))]
    [:html {:lang "en"}
     [:head
      [:title "Hyperfiddle"]
      [:link {:rel "stylesheet" :href (str resource-base "/styles.css")}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:meta {:charset "UTF-8"}]
      [:script {:id "build" :type "text/plain" :dangerouslySetInnerHTML {:__html (:BUILD env)}}]]
     [:body
      [:div
       {:id "root"
        :dangerouslySetInnerHTML
        {:__html
         (try
           (reagent-server/render-to-static-markup app-component)
           (catch :default e
             (reagent-server/render-to-string [:div
                                               [:h1 "Javascript mounting..."]
                                               [:h2 "SSR failed on:"]
                                               [error/error-block e]])))}}]
      (when (and hyperfiddle-dns? (:ANALYTICS env))
        [:div {:dangerouslySetInnerHTML {:__html analytics}}])
      (when serve-js?
        ; env vars for client side rendering
        [:script {:id "params" :type "application/transit-json"
                  :dangerouslySetInnerHTML {:__html (transit/encode params)}}])
      (when serve-js?
        [:script {:id "state" :type "application/transit-json"
                  :dangerouslySetInnerHTML {:__html (transit/encode state-val)}}])
      (when serve-js?
        [:script {:id "preamble" :src (str resource-base "/preamble.js")}])
      (when serve-js?
        [:script {:id "main" :src (str resource-base "/main.js")}])]]))

(deftype IdeSsrRuntime [host-env state-atom root-reducer jwt]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! (:service-uri host-env) jwt))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis route branch branch-aux stage]
    (hydrate-route-rpc! (:service-uri host-env) local-basis route branch branch-aux stage jwt))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! (:service-uri host-env) local-basis stage requests jwt))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! (:service-uri host-env) dbs jwt))

  runtime/AppFnRenderPageRoot
  (ssr [rt]
    (let [ctx {:peer rt
               ::runtime/branch-aux {::ide/foo "page"}}]
      [foundation/view :page ctx (if (:active-ide? host-env)
                                   (constantly [:div "loading... "])
                                   (r/partial ide/view (context/target-route ctx)))]))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this branch request]
    (unwrap #(timbre/warn %) @(hc/hydrate this branch request)))

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-edge [env req res jwt user-id path-params query-params]
  (let [initial-state {::runtime/user-id user-id}
        host-env (object/get req "host-env")
        rt (->IdeSsrRuntime host-env (r/atom (reducers/root-reducer initial-state nil)) reducers/root-reducer jwt)
        load-level foundation/LEVEL-HYDRATE-PAGE
        browser-init-level (if (:active-ide? host-env)
                             ;force the browser to re-run the data bootstrapping when not aliased
                             foundation/LEVEL-NONE
                             load-level)]
    (-> (foundation/bootstrap-data rt foundation/LEVEL-NONE load-level (.-path req) (::runtime/global-basis initial-state))
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
                  (let [serve-js? (or (:active-ide? host-env) (not @(runtime/state rt [::runtime/domain :domain/disable-javascript])))
                        params {:host-env host-env
                                :hyperfiddle.bootstrap/init-level browser-init-level}
                        html [full-html env @(runtime/state rt) serve-js? (boolean (:auth/root host-env)) params
                              (runtime/ssr rt)]]
                    (doto res
                      (.status http-status-code)
                      (.type "html")
                      (.write "<!DOCTYPE html>\n"))
                    (let [stream (render-to-node-stream html)]
                      (.on stream "error" (fn [e]
                                            (timbre/error e)
                                            (.end res (str "<h2>Fatal rendering error:</h2><h4>" (ex-message e) "</h4>"))))
                      (.pipe stream res)))))
        (p/catch (fn [e]
                   (timbre/error e)
                   (doto res
                     (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                     (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))})))))))
