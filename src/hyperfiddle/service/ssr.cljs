(ns hyperfiddle.service.ssr
  (:require
    ["react-dom/server" :as dom-server]
    [cats.monad.either :as either]
    [contrib.data :as data]
    [contrib.reactive :as r]
    [contrib.template :refer [load-resource]]
    [goog.object :as object]
    [hypercrud.browser.context :as context]
    [hypercrud.client.core :as hc]
    [hypercrud.client.peer :as peer]
    [hypercrud.transit :as transit]
    [hypercrud.ui.error :as error]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.http-client :as http-client]
    [hyperfiddle.io.local-basis :as local-basis]
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


(deftype IOImpl [service-uri build ?jwt]
  io/IO
  (global-basis [io]
    (http-client/global-basis! service-uri build ?jwt))

  (local-basis [io global-basis route]
    (p/resolved (local-basis/local-basis io global-basis route)))

  (hydrate-requests [io local-basis staged-branches requests]
    (http-client/hydrate-requests! service-uri build local-basis staged-branches requests ?jwt))

  (hydrate-route [io local-basis route branch stage]
    (http-client/hydrate-route! service-uri build local-basis route branch stage ?jwt))

  (sync [io dbnames]
    (http-client/sync! service-uri build dbnames ?jwt)))

(deftype RT [domain io state-atom root-reducer]
  runtime/State
  (dispatch! [rt action-or-func] (state/dispatch! state-atom root-reducer action-or-func))
  (state [rt] state-atom)
  (state [rt path] (r/cursor state-atom path))

  runtime/HF-Runtime
  (domain [rt] domain)
  (io [rt] io)

  hc/Peer
  (hydrate [this branch request]
    (peer/hydrate state-atom branch request))

  IHash
  (-hash [this] (goog/getUid this)))

(defn render-to-node-stream
  [component]
  (ratom/flush!)
  (binding [rutil/*non-reactive* true]
    (dom-server/renderToNodeStream (tmpl/as-element component))))

(def analytics (load-resource "analytics.html"))

(defn root-html-str [rt]
  (let [ctx {:peer rt}]
    (try
      (reagent-server/render-to-static-markup [foundation/view ctx])
      (catch :default e
        (reagent-server/render-to-string [:div
                                          [:h1 "Javascript mounting..."]
                                          [:h2 "SSR failed on:"]
                                          [error/error-block e]])))))

(defn full-html [build static-resources analytics rt client-params]
  (let [resource-base (str static-resources "/" build)]
    [:html {:lang "en"}
     [:head
      [:title "Hyperfiddle" #_(str (domain/ident (runtime/domain rt)) " - " (first @(runtime/state rt [::runtime/partitions nil :route])))]
      [:link {:rel "icon" :type "image/png" :href "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAEGWlDQ1BrQ0dDb2xvclNwYWNlR2VuZXJpY1JHQgAAOI2NVV1oHFUUPrtzZyMkzlNsNIV0qD8NJQ2TVjShtLp/3d02bpZJNtoi6GT27s6Yyc44M7v9oU9FUHwx6psUxL+3gCAo9Q/bPrQvlQol2tQgKD60+INQ6Ium65k7M5lpurHeZe58853vnnvuuWfvBei5qliWkRQBFpquLRcy4nOHj4g9K5CEh6AXBqFXUR0rXalMAjZPC3e1W99Dwntf2dXd/p+tt0YdFSBxH2Kz5qgLiI8B8KdVy3YBevqRHz/qWh72Yui3MUDEL3q44WPXw3M+fo1pZuQs4tOIBVVTaoiXEI/MxfhGDPsxsNZfoE1q66ro5aJim3XdoLFw72H+n23BaIXzbcOnz5mfPoTvYVz7KzUl5+FRxEuqkp9G/Ajia219thzg25abkRE/BpDc3pqvphHvRFys2weqvp+krbWKIX7nhDbzLOItiM8358pTwdirqpPFnMF2xLc1WvLyOwTAibpbmvHHcvttU57y5+XqNZrLe3lE/Pq8eUj2fXKfOe3pfOjzhJYtB/yll5SDFcSDiH+hRkH25+L+sdxKEAMZahrlSX8ukqMOWy/jXW2m6M9LDBc31B9LFuv6gVKg/0Szi3KAr1kGq1GMjU/aLbnq6/lRxc4XfJ98hTargX++DbMJBSiYMIe9Ck1YAxFkKEAG3xbYaKmDDgYyFK0UGYpfoWYXG+fAPPI6tJnNwb7ClP7IyF+D+bjOtCpkhz6CFrIa/I6sFtNl8auFXGMTP34sNwI/JhkgEtmDz14ySfaRcTIBInmKPE32kxyyE2Tv+thKbEVePDfW/byMM1Kmm0XdObS7oGD/MypMXFPXrCwOtoYjyyn7BV29/MZfsVzpLDdRtuIZnbpXzvlf+ev8MvYr/Gqk4H/kV/G3csdazLuyTMPsbFhzd1UabQbjFvDRmcWJxR3zcfHkVw9GfpbJmeev9F08WW8uDkaslwX6avlWGU6NRKz0g/SHtCy9J30o/ca9zX3Kfc19zn3BXQKRO8ud477hLnAfc1/G9mrzGlrfexZ5GLdn6ZZrrEohI2wVHhZywjbhUWEy8icMCGNCUdiBlq3r+xafL549HQ5jH+an+1y+LlYBifuxAvRN/lVVVOlwlCkdVm9NOL5BE4wkQ2SMlDZU97hX86EilU/lUmkQUztTE6mx1EEPh7OmdqBtAvv8HdWpbrJS6tJj3n0CWdM6busNzRV3S9KTYhqvNiqWmuroiKgYhshMjmhTh9ptWhsF7970j/SbMrsPE1suR5z7DMC+P/Hs+y7ijrQAlhyAgccjbhjPygfeBTjzhNqy28EdkUh8C+DU9+z2v/oyeH791OncxHOs5y2AtTc7nb/f73TWPkD/qwBnjX8BoJ98VQNcC+8AAACGSURBVFgJY/wTuPg/wwACpgG0G2z1qANGQ2A0BAY8BFgoKQdY1scyIusnp1Ab8BAYdQDVQuBf24H9yOmBWDYjOQmHWMOJUUe1ECDGMmxqRh1AXkHEzszAwMtxAB6kD985MHCzw7mkMMhyAMuKKIpLQJgjR9PAgIfAaEE04FEw6oDREBgNAQDZwhcK3lLErwAAAABJRU5ErkJggg==\n"}]
      [:link {:rel "stylesheet" :href (str resource-base "/styles.css")}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:meta {:charset "UTF-8"}]
      [:script {:id "build" :type "text/plain" :dangerouslySetInnerHTML {:__html build}}]]
     [:body
      [:div {:id "root" :dangerouslySetInnerHTML {:__html (root-html-str rt)}}]
      (when analytics
        [:div {:dangerouslySetInnerHTML {:__html analytics}}])
      (when-not (-> (runtime/domain rt) domain/environment :domain/disable-javascript)
        [:<>
         ; env vars for client side rendering
         [:script {:id "params" :type "application/transit-json"
                   :dangerouslySetInnerHTML {:__html (transit/encode client-params)}}]
         [:script {:id "domain" :type "application/transit-json"
                   :dangerouslySetInnerHTML {:__html (let [domain (runtime/domain rt)
                                                           f (or (:hack-transit-serializer domain) transit/encode)]
                                                       (f domain))}}]
         [:script {:id "state" :type "application/transit-json"
                   :dangerouslySetInnerHTML {:__html (transit/encode @(runtime/state rt))}}]
         [:script {:id "preamble" :src (str resource-base "/preamble.js")}]
         [:script {:id "main" :src (str resource-base "/main.js")}]])]]))

(defn bootstrap-html-cmp [env service-uri domain io path user-id]
  (let [build (:BUILD env)
        initial-state {::runtime/user-id user-id}
        rt (->RT domain io (r/atom (reducers/root-reducer initial-state nil)) reducers/root-reducer)
        static-resources (:STATIC_RESOURCES env)
        analytics (when (and (:ANALYTICS env) (contains? #{"www" "hyperfiddle"} (domain/ident (runtime/domain rt))))
                    analytics)
        load-level foundation/LEVEL-HYDRATE-PAGE]
    (-> (foundation/bootstrap-data rt foundation/LEVEL-NONE load-level path @(runtime/state rt [::runtime/global-basis]))
        (p/then (fn []
                  (let [subject @(runtime/state rt [::runtime/user-id])]
                    (->> (domain/databases (runtime/domain rt))
                         (data/map-values (fn [hf-db]
                                            false
                                            #_(either/branch
                                              (security/subject-can-transact? hf-db subject)
                                              (constantly false)
                                              identity)))
                         (into {})
                         (vector :set-auto-transact)
                         (runtime/dispatch! rt)))))
        (p/then (constantly 200))
        (p/catch #(or (:hyperfiddle.io/http-status-code (ex-data %)) 500))
        (p/then (fn [http-status-code]
                  (let [client-params {:build build
                                       :service-uri service-uri
                                       :sentry {:dsn (:SENTRY_DSN env)
                                                :environment (:SENTRY_ENV env)
                                                :release (:BUILD env)}}]
                    {:http-status-code http-status-code
                     :component [full-html build static-resources analytics rt client-params]}))))))
