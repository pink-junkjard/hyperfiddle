(ns hyperfiddle.service.http
  (:require [contrib.base-64-url-safe :as base-64-url-safe]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-edn-string]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.types.Err :refer [->Err]]
            [hyperfiddle.appval.state.reducers :as reducers]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn hyperfiddle-hostname [env request-hostname]
  (or (->> (:HF_HOSTNAMES env)
           (filter #(.endsWith request-hostname (str "." %)))
           first)
      (first (:HF_HOSTNAMES env))))

(defn e->platform-response [e]
  ; todo there are a subset of requests that are cacheable
  ; todo retry-after on 503
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :body (->Err #?(:cljs (ex-message e) :clj (.getMessage e)))})

(defn global-basis-handler [->Runtime & {:keys [hostname hyperfiddle-hostname service-uri user-profile jwt]}]
  (let [state-val (-> {:user-profile user-profile}
                      (reducers/root-reducer nil))
        rt (->Runtime hyperfiddle-hostname hostname service-uri (r/atom state-val) reducers/root-reducer jwt)]
    (-> (runtime/global-basis rt)
        (p/then (fn [global-basis]
                  {:status 200
                   :headers {"Cache-Control" "max-age=0"}
                   :body global-basis}))
        (p/catch (fn [e]
                   (timbre/error e)
                   (e->platform-response e))))))

(defn local-basis-handler [->Runtime & {:keys [hostname path-params hyperfiddle-hostname service-uri user-profile jwt]}]
  (try
    (let [global-basis (-> (:global-basis path-params) base-64-url-safe/decode read-edn-string) ; todo this can throw
          route (-> (:encoded-route path-params) base-64-url-safe/decode read-edn-string)
          _ (when-let [e (routing/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) base-64-url-safe/decode read-edn-string)
          branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode read-edn-string)
          initial-state {:user-profile user-profile
                         ::runtime/global-basis global-basis
                         ::runtime/partitions {branch {:route route
                                                       :hyperfiddle.runtime/branch-aux branch-aux}}}
          rt (->Runtime hyperfiddle-hostname hostname service-uri
                        (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt)]
      (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then (fn [_] (runtime/local-basis rt global-basis route branch branch-aux)))
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

(defn hydrate-route-handler [->Runtime & {:keys [hostname path-params request-body hyperfiddle-hostname service-uri user-profile jwt]}]
  (try
    (let [local-basis (-> (:local-basis path-params) base-64-url-safe/decode read-edn-string) ; todo this can throw
          route (-> (:encoded-route path-params) base-64-url-safe/decode read-edn-string)
          _ (when-let [e (routing/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) base-64-url-safe/decode read-edn-string)
          branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode read-edn-string)
          initial-state (-> {:user-profile user-profile
                             :stage request-body
                             ; should this be constructed with reducers?
                             ; why dont we need to preheat the tempid lookups here for parent branches?
                             ::runtime/partitions {branch {:local-basis local-basis
                                                           :route route
                                                           :hyperfiddle.runtime/branch-aux branch-aux}}})
          rt (->Runtime hyperfiddle-hostname hostname service-uri
                        (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt)]
      (-> (foundation-actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then (fn [_] (runtime/hydrate-route rt local-basis route branch branch-aux (:stage initial-state))))
          (p/then (fn [data]
                    {:status 200
                     :headers {"Cache-Control" "max-age=31536000"} ; todo max-age=0 if POST
                     :body data}))
          (p/catch (fn [e]
                     (timbre/error e)
                     (e->platform-response e)))))
    (catch #?(:cljs :default :clj Exception) e
      (timbre/error e)
      (p/resolved (e->platform-response e)))))
