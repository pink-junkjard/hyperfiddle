(ns hyperfiddle.service.http
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [contrib.reader :refer [read-edn-string!]]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defmulti handle-route (fn [handler & rest] handler))

(defn e->platform-response [e]
  ; todo there are a subset of requests that are cacheable
  ; todo retry-after on 503
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :body (->Err #?(:cljs (ex-message e) :clj (.getMessage e)))})

(defn global-basis-handler [->IO & {:keys [domain user-id jwt]}]
  (-> (->IO domain jwt user-id)
      (io/global-basis)
      (p/then (fn [global-basis]
                {:status 200
                 :headers {"Cache-Control" "max-age=0"}
                 :body global-basis}))
      (p/catch (fn [e]
                 (timbre/error e)
                 (e->platform-response e)))))

(defn local-basis-handler [->IO & {:keys [domain route-params user-id jwt]}]
  (try
    (let [global-basis (ednish/decode-uri (:global-basis route-params)) ; todo this can throw
          route (-> (:encoded-route route-params) base-64-url-safe/decode read-edn-string!)
          ; todo this needs work, decoding should happen after the domain is hydrated
          #_#_route (-> (str "/" (:encoded-route route-params)) (foundation/route-decode rt))
          _ (either/branch
              (route/validate-route+ route)
              (fn [e] (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e)))
              (constantly nil))]
      (-> (->IO domain jwt user-id)
          (io/local-basis global-basis route)
          (p/then (fn [local-basis]
                    {:status 200
                     :headers {"Cache-Control" "max-age=31536000"}
                     :body local-basis}))
          (p/catch (fn [e]
                     (timbre/error e)
                     (e->platform-response e)))))
    (catch #?(:cljs :default :clj Exception) e
      (timbre/error e)
      (p/resolved (e->platform-response e)))))

(defn hydrate-route-handler [->IO & {:keys [domain route-params user-id jwt] stage :request-body}]
  (try
    (let [io (->IO domain jwt user-id)
          local-basis (ednish/decode-uri (:local-basis route-params)) ; todo this decoding can throw
          route (-> (:encoded-route route-params) base-64-url-safe/decode read-edn-string!)
          ; todo this needs work, decoding should happen after the domain is hydrated
          #_#_route (-> (str "/" (:encoded-route route-params)) (foundation/route-decode rt))
          _ (either/branch
              (route/validate-route+ route)
              (fn [e] (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e)))
              (constantly nil))
          branch (some-> (:branch route-params) ednish/decode-uri)]
      (-> (->> (mapcat second stage)
               (remove (comp empty? second))
               (map first)
               (distinct)
               (io/sync io))
          (p/then (fn [sync-results]
                    (let [local-basis (reduce-kv assoc (into {} local-basis) sync-results)]
                      (io/hydrate-route io local-basis route branch stage))))
          (p/then (fn [data]
                    (let [cache-control (if (some seq (vals stage))
                                          "max-age=0"
                                          "max-age=31536000")]
                      {:status 200
                       :headers {"Cache-Control" cache-control}
                       :body data})))
          (p/catch (fn [e]
                     (timbre/error e)
                     (e->platform-response e)))))
    (catch #?(:cljs :default :clj Exception) e
      (timbre/error e)
      (p/resolved (e->platform-response e)))))

(defn ssr-auth-hack [domain user-id path redirect next]
  (if (and (= "demo" (:app-domain-ident domain))
           (nil? user-id)
           (not (string/starts-with? path "/:hyperfiddle.ide!please-login/")))
    ; todo this logic should be injected into demo domain record
    (let [inner-route (domain/url-decode domain path)
          url (domain/url-encode domain [:hyperfiddle.ide/please-login inner-route])]
      (redirect url))
    (next)))
