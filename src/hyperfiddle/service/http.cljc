(ns hyperfiddle.service.http
  (:require
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]

    [taoensso.timbre :as timbre]
    [cats.monad.either :as either]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.uri :refer [->URI]]
    [promesa.core :as p]))


(defn e->platform-response [e]
  ; todo there are a subset of requests that are cacheable
  ; todo retry-after on 503
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :body (->Err (ex-message e))})

(defn global-basis-handler [->IO & {:keys [domain service-uri user-id jwt]}]
  (-> (->IO domain service-uri jwt user-id)
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

(defn hydrate-route-handler [->IO & {:keys [domain route-params user-id jwt] partitions :request-body}]
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
          pid (-> (:partition-id route-params) base-64-url-safe/decode read-edn-string!)]
      (-> (->> (vals partitions)
               (mapcat :stage)
               (remove (comp empty? second))
               (map first)
               (filter #(domain/database domain %))         ; only sync declared dbnames
               (distinct)
               (io/sync io))
          (p/then (fn [sync-results]
                    (let [local-basis (reduce-kv assoc (into {} local-basis) sync-results)]
                      (io/hydrate-route io local-basis route pid partitions))))
          (p/then (fn [data]
                    (let [cache-control (if true #_(some seq (vals stage)) ; todo
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

