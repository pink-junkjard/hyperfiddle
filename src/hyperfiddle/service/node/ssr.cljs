(ns hyperfiddle.service.node.ssr
  (:require [cljs.nodejs :as node]

            [hypercrud.client.core :as hc]
            [hypercrud.client.peer :as peer]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.state.core :as state]
            [hypercrud.transit :as transit]
            [hypercrud.util.core :as util]
            [hypercrud.util.exception :refer [->Exception]]
            [hypercrud.util.performance :as perf]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.template :as template]

            [hyperfiddle.api :as api]
            [hyperfiddle.appfn.runtime-rpc :refer [hydrate-requests! sync! transact!!]]
            [hyperfiddle.appval.runtime-local :refer [hydrate-route global-basis local-basis]]
            [hyperfiddle.appval.runtime-rpc :refer [hydrate-route! global-basis! local-basis!]]
            [hyperfiddle.appval.domain.app-ui :as ide]
            [hyperfiddle.appval.state.reducers :as reducers]

            [promesa.core :as p]
            [reagent.dom.server :as reagent-server]
            [taoensso.timbre :as timbre]
            ))

(def cheerio (node/require "cheerio"))


(defn local-html [F ctx]                                    ; react 16 is async, and this can fail
  ; html fragment, not a document, no <html> enclosing tag
  (p/resolved
    (let [ctx (assoc ctx :dispatch! #(throw (->Exception "dispatch! not supported in ssr")))]
      (perf/time (fn [get-total-time] (timbre/debug "Render total time:" (get-total-time)))
                 (reagent-server/render-to-string (F ctx))))))

(def analytics (template/load-resource "analytics.html"))

(defn error [error]
  (str "<html><body><h2>Error:</h2><pre>"
       (util/pprint-str error 100)
       "</pre></body></html>"))

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

(defn evaluated-template [env state-val params app-html]
  (let [$ (.load cheerio template)
        resource-base (str (:STATIC_RESOURCES env) "/" (:BUILD env))]
    (-> ($ "title") (.text "Hyperfiddle"))
    (-> ($ "#app-css") (.attr "href" (str resource-base "/styles.css")))
    (-> ($ "#root") (.html app-html))
    (-> ($ "#params") (.text (transit/encode params)))      ; env vars for client side rendering
    (-> ($ "#state") (.text (transit/encode state-val)))
    (-> ($ "#preamble") (.attr "src" (str resource-base "/preamble.js")))
    (-> ($ "#main") (.attr "src" (str resource-base "/main.js")))
    (-> ($ "#build") (.text (:BUILD env)))
    (when (:ANALYTICS env) (-> ($ "#root") (.after analytics))) ; if logged in, issue identify?
    (.html $)))

(defn html [env state-val app-html]
  (let [params {:hyperfiddle-hostname (:HF_HOSTNAME env)}]
    (perf/time (fn [get-total-time] (timbre/debug "Template total time:" (get-total-time)))
               (evaluated-template env state-val params app-html))))

(defn ssr [env rt hyperfiddle-hostname hostname]
  (let [state-atom (.-state-atom rt)
        get-state (fn [] @state-atom)
        dispatch! (state/build-dispatch state-atom reducers/root-reducer)]
    (-> (actions/refresh-global-basis rt dispatch! get-state)
        (p/then #(actions/refresh-page-local-basis rt dispatch! get-state))
        (p/then #(actions/hydrate-page rt nil dispatch! get-state))
        (p/catch (constantly (p/resolved nil)))             ; any error above IS NOT fatal, so render the UI. anything below IS fatal
        (p/then #(local-html ide/ui {:hostname hostname
                                     :hyperfiddle-hostname hyperfiddle-hostname
                                     :peer rt}))
        (p/then (fn [html-fragment] (html env @state-atom html-fragment)))
        (p/catch (fn [error]
                   (timbre/error error)
                   ; careful this needs to throw a Throwable in clj
                   (p/rejected (error error)))))))

(deftype EdgeApiImpl [hyperfiddle-hostname hostname service-uri state-atom]
  api/AppValGlobalBasis
  (global-basis [rt]
    (global-basis! service-uri))

  api/AppValLocalBasis
  (local-basis [rt global-basis encoded-route foo branch]
    (local-basis rt hyperfiddle-hostname hostname global-basis encoded-route foo))

  api/AppValHydrate
  (hydrate-route [rt local-basis encoded-route foo branch stage]
    (hydrate-route! service-uri local-basis encoded-route foo branch stage))

  api/AppFnHydrate
  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests! service-uri local-basis stage requests))

  api/AppFnSync
  (sync [rt dbs]
    (sync! service-uri dbs))

  hc/Peer
  (hydrate [this request]
    (peer/hydrate state-atom request))

  (db [this uri branch]
    (peer/db-pointer state-atom uri branch))

  ; IEquiv?

  IHash
  (-hash [this] (goog/getUid this)))

(defn http-edge [env req res path-params query-params]
  (let [hostname (.-hostname req)
        state-val (req->state-val env req path-params query-params)
        rt (EdgeApiImpl. (:HF_HOSTNAME env) hostname (req->service-uri env req) (reactive/atom state-val))]
    (-> (ssr env rt (:HF_HOSTNAME env) hostname)
        (p/then (fn [html-resp]
                  (doto res
                    (.append "Cache-Control" "max-age=0")
                    (.status 200)
                    (.format #js {"text/html" #(.send res html-resp)}))))
        (p/catch (fn [error-resp]
                   (doto res
                     (.status 500)
                     (.format #js {"text/html" #(.send res error-resp)})))))))
