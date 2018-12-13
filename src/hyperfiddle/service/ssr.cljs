(ns hyperfiddle.service.ssr
  (:require
    ["react-dom/server" :as dom-server]
    [cats.monad.either :as either]
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
    [hyperfiddle.ui.loading :refer [loading-page]]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [reagent.impl.template :as tmpl]
    [reagent.impl.util :as rutil]
    [reagent.ratom :as ratom]
    [taoensso.timbre :as timbre]))


(deftype RT [host-env state-atom root-reducer jwt]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HostInfo
  (host-env [rt] host-env)

  runtime/DomainRegistry
  (domain [rt]
    (ide/domain rt (:domain-eid host-env)))

  runtime/IO
  (global-basis [rt]
    (global-basis-rpc! (:service-uri host-env) (:build host-env) jwt))

  (local-basis [rt branch]
    (let [global-basis @(runtime/state rt [::runtime/global-basis])
          {:keys [route ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          ctx {:branch branch
               ::runtime/branch-aux branch-aux
               :peer rt}
          ; this is ide
          page-or-leaf (case (:hyperfiddle.ide/foo branch-aux)
                         "page" :page
                         "user" :leaf
                         "ide" :leaf)]
      (foundation/local-basis page-or-leaf global-basis route ctx ide/local-basis)))

  (hydrate-route [rt branch]
    (let [{:keys [route local-basis ::runtime/branch-aux]} @(runtime/state rt [::runtime/partitions branch])
          stage (map-values :stage @(runtime/state rt [::runtime/partitions]))]
      (hydrate-route-rpc! (:service-uri host-env) (:build host-env) local-basis route branch branch-aux stage jwt)))

  (hydrate-requests [rt local-basis stage requests]
    (hydrate-requests-rpc! (:service-uri host-env) (:build host-env) local-basis stage requests jwt))

  (sync [rt dbs]
    (sync-rpc! (:service-uri host-env) (:build host-env) dbs jwt))

  runtime/Schema
  (hydrate-schemas [rt branch]
    (ide/hydrate-schemas rt branch))

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  (db [this uri branch]
    (peer/db-pointer uri branch))

  IHash
  (-hash [this] (goog/getUid this)))

(defn build-runtime [host-env initial-state jwt]
  (->RT host-env (r/atom (reducers/root-reducer initial-state nil)) reducers/root-reducer jwt))


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
      [:link {:rel "icon" :type "image/png" :href "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAEGWlDQ1BrQ0dDb2xvclNwYWNlR2VuZXJpY1JHQgAAOI2NVV1oHFUUPrtzZyMkzlNsNIV0qD8NJQ2TVjShtLp/3d02bpZJNtoi6GT27s6Yyc44M7v9oU9FUHwx6psUxL+3gCAo9Q/bPrQvlQol2tQgKD60+INQ6Ium65k7M5lpurHeZe58853vnnvuuWfvBei5qliWkRQBFpquLRcy4nOHj4g9K5CEh6AXBqFXUR0rXalMAjZPC3e1W99Dwntf2dXd/p+tt0YdFSBxH2Kz5qgLiI8B8KdVy3YBevqRHz/qWh72Yui3MUDEL3q44WPXw3M+fo1pZuQs4tOIBVVTaoiXEI/MxfhGDPsxsNZfoE1q66ro5aJim3XdoLFw72H+n23BaIXzbcOnz5mfPoTvYVz7KzUl5+FRxEuqkp9G/Ajia219thzg25abkRE/BpDc3pqvphHvRFys2weqvp+krbWKIX7nhDbzLOItiM8358pTwdirqpPFnMF2xLc1WvLyOwTAibpbmvHHcvttU57y5+XqNZrLe3lE/Pq8eUj2fXKfOe3pfOjzhJYtB/yll5SDFcSDiH+hRkH25+L+sdxKEAMZahrlSX8ukqMOWy/jXW2m6M9LDBc31B9LFuv6gVKg/0Szi3KAr1kGq1GMjU/aLbnq6/lRxc4XfJ98hTargX++DbMJBSiYMIe9Ck1YAxFkKEAG3xbYaKmDDgYyFK0UGYpfoWYXG+fAPPI6tJnNwb7ClP7IyF+D+bjOtCpkhz6CFrIa/I6sFtNl8auFXGMTP34sNwI/JhkgEtmDz14ySfaRcTIBInmKPE32kxyyE2Tv+thKbEVePDfW/byMM1Kmm0XdObS7oGD/MypMXFPXrCwOtoYjyyn7BV29/MZfsVzpLDdRtuIZnbpXzvlf+ev8MvYr/Gqk4H/kV/G3csdazLuyTMPsbFhzd1UabQbjFvDRmcWJxR3zcfHkVw9GfpbJmeev9F08WW8uDkaslwX6avlWGU6NRKz0g/SHtCy9J30o/ca9zX3Kfc19zn3BXQKRO8ud477hLnAfc1/G9mrzGlrfexZ5GLdn6ZZrrEohI2wVHhZywjbhUWEy8icMCGNCUdiBlq3r+xafL549HQ5jH+an+1y+LlYBifuxAvRN/lVVVOlwlCkdVm9NOL5BE4wkQ2SMlDZU97hX86EilU/lUmkQUztTE6mx1EEPh7OmdqBtAvv8HdWpbrJS6tJj3n0CWdM6busNzRV3S9KTYhqvNiqWmuroiKgYhshMjmhTh9ptWhsF7970j/SbMrsPE1suR5z7DMC+P/Hs+y7ijrQAlhyAgccjbhjPygfeBTjzhNqy28EdkUh8C+DU9+z2v/oyeH791OncxHOs5y2AtTc7nb/f73TWPkD/qwBnjX8BoJ98VQNcC+8AAACGSURBVFgJY/wTuPg/wwACpgG0G2z1qANGQ2A0BAY8BFgoKQdY1scyIusnp1Ab8BAYdQDVQuBf24H9yOmBWDYjOQmHWMOJUUe1ECDGMmxqRh1AXkHEzszAwMtxAB6kD985MHCzw7mkMMhyAMuKKIpLQJgjR9PAgIfAaEE04FEw6oDREBgNAQDZwhcK3lLErwAAAABJRU5ErkJggg==\n"}]
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

(defn bootstrap-html-cmp [env rt path]
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
