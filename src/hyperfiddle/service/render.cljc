(ns hyperfiddle.service.render
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :as either]
    [contrib.data :as data]
    [contrib.reactive :as r]
    [contrib.template :refer [load-resource]]
    [clojure.spec.alpha :as s]
    [hypercrud.transit :as hc-t]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.runtime-impl :as runtime-impl]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.state :as state]
    #?(:cljs [hyperfiddle.ui.error :as ui-error])
    [hyperfiddle.ui.loading :as loading]
    [promesa.core :as p]
    #?(:cljs [reagent.dom.server :as reagent-server])
    [hyperfiddle.service.resolve :as R]
    [taoensso.timbre :as timbre]))


(deftype RT [domain io state-atom]
  state/State
  (state [rt] state-atom)

  runtime/HF-Runtime
  (domain [rt] domain)
  (io [rt] io)
  (hydrate [rt pid request] (runtime-impl/hydrate-impl rt pid request))
  (set-route [rt pid route] (runtime-impl/set-route rt pid route false))
  (set-route [rt pid route force-hydrate] (runtime-impl/set-route rt pid route force-hydrate)))

(declare main-html inner-html)

(defn render [config domain io route user-id]
  (let [state {::runtime/auto-transact (data/map-values
                                         (fn [hf-db]
                                           (if-some [auto-tx (:database/auto-transact hf-db)]
                                             auto-tx
                                             (either/right? (security/subject-can-transact? hf-db user-id))))
                                         (domain/databases domain))
               ::runtime/partitions    {foundation/root-pid {:route       route
                                                             :is-branched true}}
               ::runtime/user-id       user-id}

        _ (s/assert domain/spec-ednish-domain domain)
        rt (->RT domain io (r/atom (state/initialize state)))]
    (-> (runtime/bootstrap-data rt foundation/root-pid runtime/LEVEL-NONE)
        (p/catch #(do (timbre/error %)
                      (or (:hyperfiddle.io/http-status-code (ex-data %))) 500))
        (p/then (fn [http-status-code]
                  {:http-status-code (or http-status-code 200)
                   :component [main-html config rt]})))))

(defn restrict-inner-map-vals
  "Given a map of maps, restrict the inner maps by ks"
  [m & [ks]]
  (reduce-kv (fn [acc k v]
               ; v is inner map
               ; (select-keys {:a 1 :b 2} nil) => {}
               (assoc acc k (select-keys v ks)))
    {} m))

(def db-allowed-keys [:database/db-name :database/write-security :hf/transaction-operation-whitelist])

(defn omit-secure-keys [domain]
  (-> domain
    (update-in [:config] dissoc :auth0)
    (update-in [:config :domain :databases] restrict-inner-map-vals db-allowed-keys)
    (update-in [:databases] restrict-inner-map-vals db-allowed-keys)
    (update-in [:hyperfiddle.ide.domain/user-domain+]
      (fn [v+]
        (fmap (fn [v]
                (-> v
                  (update-in [:config] dissoc :auth0)
                  (update-in [:config :domain :databases] restrict-inner-map-vals db-allowed-keys)
                  (update-in [:databases] restrict-inner-map-vals db-allowed-keys)))
          v+)))))
; Test this with a deep-def in main-html

(defn- main-html [config rt]
  ;(def dustin (runtime/domain rt))
  [:html {:lang "en"}
   [:head
    [:title "Hyperfiddle" #_(str (:hyperfiddle.route/fiddle (runtime/get-route rt foundation/root-pid)))]
    [:link {:rel "icon" :type "image/png" :href "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAEGWlDQ1BrQ0dDb2xvclNwYWNlR2VuZXJpY1JHQgAAOI2NVV1oHFUUPrtzZyMkzlNsNIV0qD8NJQ2TVjShtLp/3d02bpZJNtoi6GT27s6Yyc44M7v9oU9FUHwx6psUxL+3gCAo9Q/bPrQvlQol2tQgKD60+INQ6Ium65k7M5lpurHeZe58853vnnvuuWfvBei5qliWkRQBFpquLRcy4nOHj4g9K5CEh6AXBqFXUR0rXalMAjZPC3e1W99Dwntf2dXd/p+tt0YdFSBxH2Kz5qgLiI8B8KdVy3YBevqRHz/qWh72Yui3MUDEL3q44WPXw3M+fo1pZuQs4tOIBVVTaoiXEI/MxfhGDPsxsNZfoE1q66ro5aJim3XdoLFw72H+n23BaIXzbcOnz5mfPoTvYVz7KzUl5+FRxEuqkp9G/Ajia219thzg25abkRE/BpDc3pqvphHvRFys2weqvp+krbWKIX7nhDbzLOItiM8358pTwdirqpPFnMF2xLc1WvLyOwTAibpbmvHHcvttU57y5+XqNZrLe3lE/Pq8eUj2fXKfOe3pfOjzhJYtB/yll5SDFcSDiH+hRkH25+L+sdxKEAMZahrlSX8ukqMOWy/jXW2m6M9LDBc31B9LFuv6gVKg/0Szi3KAr1kGq1GMjU/aLbnq6/lRxc4XfJ98hTargX++DbMJBSiYMIe9Ck1YAxFkKEAG3xbYaKmDDgYyFK0UGYpfoWYXG+fAPPI6tJnNwb7ClP7IyF+D+bjOtCpkhz6CFrIa/I6sFtNl8auFXGMTP34sNwI/JhkgEtmDz14ySfaRcTIBInmKPE32kxyyE2Tv+thKbEVePDfW/byMM1Kmm0XdObS7oGD/MypMXFPXrCwOtoYjyyn7BV29/MZfsVzpLDdRtuIZnbpXzvlf+ev8MvYr/Gqk4H/kV/G3csdazLuyTMPsbFhzd1UabQbjFvDRmcWJxR3zcfHkVw9GfpbJmeev9F08WW8uDkaslwX6avlWGU6NRKz0g/SHtCy9J30o/ca9zX3Kfc19zn3BXQKRO8ud477hLnAfc1/G9mrzGlrfexZ5GLdn6ZZrrEohI2wVHhZywjbhUWEy8icMCGNCUdiBlq3r+xafL549HQ5jH+an+1y+LlYBifuxAvRN/lVVVOlwlCkdVm9NOL5BE4wkQ2SMlDZU97hX86EilU/lUmkQUztTE6mx1EEPh7OmdqBtAvv8HdWpbrJS6tJj3n0CWdM6busNzRV3S9KTYhqvNiqWmuroiKgYhshMjmhTh9ptWhsF7970j/SbMrsPE1suR5z7DMC+P/Hs+y7ijrQAlhyAgccjbhjPygfeBTjzhNqy28EdkUh8C+DU9+z2v/oyeH791OncxHOs5y2AtTc7nb/f73TWPkD/qwBnjX8BoJ98VQNcC+8AAACGSURBVFgJY/wTuPg/wwACpgG0G2z1qANGQ2A0BAY8BFgoKQdY1scyIusnp1Ab8BAYdQDVQuBf24H9yOmBWDYjOQmHWMOJUUe1ECDGMmxqRh1AXkHEzszAwMtxAB6kD985MHCzw7mkMMhyAMuKKIpLQJgjR9PAgIfAaEE04FEw6oDREBgNAQDZwhcK3lLErwAAAABJRU5ErkJggg==\n"}]
    [:link {:rel "stylesheet" :href (domain/api-path-for (runtime/domain rt) :static-resource :build (:git/describe config) :resource-name "browser.css")}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:meta {:charset "UTF-8"}]
    (inner-html :script {:id "build" :type "text/plain"} (:git/describe config))]

   [:body
    (inner-html :div {:id (or (:html-root-id (runtime/domain rt)) "root")}
      (loading/page (runtime/domain rt)))
    (when (not (-> rt runtime/domain domain/environment :domain/disable-javascript))
      (concat
        (map (fn [[id val]] (inner-html :script {:id id :type "application/transit-json"} val))
          {#_#_"params" (hc-t/encode (:CLIENT_PARAMS env))
           "domain" (-> (runtime/domain rt) omit-secure-keys hc-t/encode)
           "state"  (hc-t/encode @(state/state rt))})
        [[:script {:id "preamble" :src (domain/api-path-for (runtime/domain rt) :static-resource :build (:git/describe config) :resource-name "browser.js")}]
         [:script {:id "main" :src (domain/api-path-for (runtime/domain rt) :static-resource :build (:git/describe config) :resource-name "main.js")}]]))]])

(defn- inner-html
  ([tag html]
   (inner-html tag {} html))
  ([tag props html]
   #?(:clj  [tag props html]
      :cljs [tag (assoc-in props [:dangerouslySetInnerHTML :__html] html)])))

(defn root-html-str [rt]
  (let [ctx (map->Context {:runtime rt
                           :partition-id foundation/root-pid})]
    #?(:clj  (loading/page (runtime/domain (:runtime ctx)))
       :cljs (try
               (reagent-server/render-to-static-markup [foundation/view ctx])
               (catch :default e
                 (reagent-server/render-to-string [:<>
                                                   [:h1 "Javascript mounting..."]
                                                   [:h2 "SSR failed on:"]
                                                   [ui-error/error-block e]]))))))



