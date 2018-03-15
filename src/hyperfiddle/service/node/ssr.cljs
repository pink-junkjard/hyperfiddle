(ns hyperfiddle.service.node.ssr
  (:require [cats.core :refer [mlet return]]
            [cljs.nodejs :as node]
            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.transit :as transit]
            [hypercrud.util.core :as util :refer [unwrap]]
            [hypercrud.util.performance :as perf]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.template :as template]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.ide :as ide]
            [hyperfiddle.io.global-basis :refer [global-basis-rpc!]]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-requests-rpc!]]
            [hyperfiddle.io.hydrate-route :refer [hydrate-route-rpc!]]
            [hyperfiddle.io.sync :refer [sync-rpc!]]
            [hyperfiddle.runtime :as runtime]
            [hyperfiddle.service.node.lib :as lib :refer [req->service-uri]]
            [hyperfiddle.state :as state]
            [promesa.core :as p]
            [reagent.dom.server :as reagent-server]
            [taoensso.timbre :as timbre]))

(def cheerio (node/require "cheerio"))


(defn render-local-html [F]                                 ; react 16 is async, and this can fail
  ; html fragment, not a document, no <html> enclosing tag
  (p/resolved
    (perf/time (fn [get-total-time] (timbre/debug "Render total time:" (get-total-time)))
               (reagent-server/render-to-string (F)))))

(def analytics (template/load-resource "analytics.html"))

(def template
  "
  <!DOCTYPE html>
  <html lang='en'>
  <head>
    <title></title>
    <link id='app-css' rel='stylesheet' type='text/css'>
    <meta name='viewport' content='width=device-width,initial-scale=1'>
    <meta charset='UTF-8'>
    <script id='build' type='text/edn'></script>
  </head>
  <body>
    <div id='root'></div>
    <script id='params' type='text/edn'></script>
    <script id='state' type='text/edn'></script>
    <script id='preamble' type='text/javascript'></script>
    <script id='main' type='text/javascript'></script>
  </body>
  </html>")

(defn evaluated-template [env state-val params serve-js? app-html]
  (let [$ (.load cheerio template)
        resource-base (str (:STATIC_RESOURCES env) "/" (:BUILD env))]
    (-> ($ "title") (.text "Hyperfiddle"))
    (-> ($ "#app-css") (.attr "href" (str resource-base "/styles.css")))
    (-> ($ "#root") (.html app-html))
    (when serve-js?
      (-> ($ "#params") (.text (transit/encode params)))    ; env vars for client side rendering
      (-> ($ "#state") (.text (transit/encode state-val)))
      (-> ($ "#preamble") (.attr "src" (str resource-base "/preamble.js")))
      (-> ($ "#main") (.attr "src" (str resource-base "/main.js"))))
    (-> ($ "#build") (.text (:BUILD env)))
    (when (:ANALYTICS env) (-> ($ "#root") (.after analytics))) ; if logged in, issue identify?
    (.html $)))

(defn html [env state-val serve-js? app-html]
  (let [params {:hyperfiddle-hostname (:HF_HOSTNAME env)}]
    (perf/time (fn [get-total-time] (timbre/debug "Template total time:" (get-total-time)))
               (evaluated-template env state-val params serve-js? app-html))))

(deftype IdeSsrRuntime [hyperfiddle-hostname hostname service-uri state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (reactive/cursor state-atom path))

  runtime/AppFnGlobalBasis
  (global-basis [rt]
    (global-basis-rpc! service-uri))

  runtime/Route
  (decode-route [rt s]
    (ide/route-decode rt s))

  (encode-route [rt v]
    (ide/route-encode rt v))

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt hyperfiddle-hostname hostname))

  runtime/AppValLocalBasis
  (local-basis [rt global-basis route branch branch-aux]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :branch branch
               :hyperfiddle.runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  runtime/AppValHydrate
  (hydrate-route [rt local-basis route branch branch-aux stage]
    (hydrate-route-rpc! service-uri local-basis route branch branch-aux stage))

  runtime/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! service-uri local-basis stage requests))

  runtime/AppFnSync
  (sync [rt dbs]
    (sync-rpc! service-uri dbs))

  runtime/AppFnRenderPageRoot
  (ssr [rt-page route]
    (let [ctx {:hostname hostname
               :hyperfiddle-hostname hyperfiddle-hostname
               :peer rt-page
               ::runtime/branch-aux {::ide/foo "page"}}]
      (render-local-html
        (partial foundation/view :page route ctx ide/view))))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  hc/HydrateApi
  (hydrate-api [this branch request]
    (unwrap @(hc/hydrate this branch request)))

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-edge [env req res path-params query-params]
  (let [hostname (.-hostname req)
        user-profile (lib/req->user-profile env req)
        initial-state {:user-profile user-profile}
        rt (->IdeSsrRuntime (:HF_HOSTNAME env) hostname (req->service-uri env req)
                            (reactive/atom (reducers/root-reducer initial-state nil))
                            reducers/root-reducer)
        serve-js? true #_(not aliased?)
        browser-initial-state (fn []
                                (let [bootstrapped-state @(runtime/state rt)
                                      alias? (foundation/alias? (foundation/hostname->hf-domain-name hostname (:HF_HOSTNAME env)))]
                                  ; initial-state to force the browser to re-run the data bootstrapping when not aliased
                                  (if alias?
                                    bootstrapped-state
                                    (merge initial-state (select-keys bootstrapped-state [::runtime/auto-transact])))))
        ; careful setting load-level to LOCAL-BASIS; it is not supported as an init-level in the browser yet
        ; how can the browser know if hydrate page has or has not happened yet?
        load-level foundation/LEVEL-HYDRATE-PAGE]
    (-> (foundation/bootstrap-data rt foundation/LEVEL-NONE load-level (.-path req))
        (p/then (fn []
                  (let [action (if (or (foundation/alias? (foundation/hostname->hf-domain-name hostname (:HF_HOSTNAME env)))
                                       (foundation/domain-owner? user-profile @(runtime/state rt [::runtime/domain])))
                                 [:enable-auto-transact]
                                 [:disable-auto-transact])]
                    (runtime/dispatch! rt action))))
        (p/catch (fn [error]
                   #_"any error above IS NOT fatal, so render the UI. anything below IS fatal"
                   (timbre/error error)
                   (p/resolved nil)))
        (p/then #(runtime/ssr rt @(runtime/state rt [::runtime/partitions nil :route])))
        (p/then (fn [html-fragment] (html env (browser-initial-state) serve-js? html-fragment)))
        (p/catch (fn [error]
                   (timbre/error error)
                   (let [html-fragment (str "<h2>Error:</h2><pre>" (util/pprint-str error 100) "</pre>")]
                     ; careful this needs to throw a Throwable in clj
                     (p/rejected (html env (browser-initial-state) serve-js? html-fragment)))))
        (p/then (fn [html-resp]
                  (doto res
                    (.append "Cache-Control" "max-age=0")
                    (.status 200)
                    (.format #js {"text/html" #(.send res html-resp)}))))
        (p/catch (fn [error-resp]
                   (doto res
                     (.status 500)
                     (.format #js {"text/html" #(.send res error-resp)})))))))
