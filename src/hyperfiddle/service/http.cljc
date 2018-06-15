(ns hyperfiddle.service.http
  (:require [contrib.base-64-url-safe :as base-64-url-safe]
            [contrib.reactive :as r]
            [contrib.reader :refer [read-edn-string]]
            [hypercrud.browser.router :as router]
            [hypercrud.types.Err :refer [->Err]]
            [hypercrud.types.URI :refer [->URI]]
            [hyperfiddle.actions :as actions]
            [hyperfiddle.reducers :as reducers]
            [hyperfiddle.runtime :as runtime :refer [map->HostEnvironment]]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


; hf.net cloud specific
(defn cloud-host-environment [{:keys [BUILD HF_HOSTNAMES HF_ALIAS_HOSTNAMES] :as env} protocol hostname]
  {:pre [hostname]}
  (let [hf-hostname (first (filter #(.endsWith hostname (str "." %)) HF_HOSTNAMES))
        alias-hostname (first (filter #(.endsWith hostname (str "." %)) HF_ALIAS_HOSTNAMES))]
    (map->HostEnvironment
      {:hostname hostname
       :service-uri (->URI (str protocol "://" hostname "/api/" BUILD "/"))
       :active-ide? (boolean hf-hostname)
       :domain-eid (cond
                     hf-hostname [:domain/ident (second (re-find (re-pattern (str "(.*)\\." hf-hostname)) hostname))]
                     alias-hostname [:domain/ident (second (re-find (re-pattern (str "(.*)\\." alias-hostname)) hostname))]
                     :else [:domain/aliases hostname])
       :auth/root (or hf-hostname alias-hostname)
       :ide/root (or hf-hostname (first HF_ALIAS_HOSTNAMES))})))

(defn e->platform-response [e]
  ; todo there are a subset of requests that are cacheable
  ; todo retry-after on 503
  {:status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
   :body (->Err #?(:cljs (ex-message e) :clj (.getMessage e)))})

(defn global-basis-handler [->Runtime & {:keys [host-env user-id jwt]}]
  (let [state-val (-> {::runtime/user-id user-id}
                      (reducers/root-reducer nil))
        rt (->Runtime host-env (r/atom state-val) reducers/root-reducer jwt user-id)]
    (-> (runtime/global-basis rt)
        (p/then (fn [global-basis]
                  {:status 200
                   :headers {"Cache-Control" "max-age=0"}
                   :body global-basis}))
        (p/catch (fn [e]
                   (timbre/error e)
                   (e->platform-response e))))))

(defn local-basis-handler [->Runtime & {:keys [host-env path-params user-id jwt]}]
  (try
    (let [global-basis (-> (:global-basis path-params) base-64-url-safe/decode read-edn-string) ; todo this can throw
          route (-> (:encoded-route path-params) base-64-url-safe/decode read-edn-string)
          _ (when-let [e (router/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) base-64-url-safe/decode read-edn-string)
          branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode read-edn-string)
          initial-state {::runtime/user-id user-id
                         ::runtime/global-basis global-basis
                         ::runtime/partitions {branch {:route route
                                                       :hyperfiddle.runtime/branch-aux branch-aux}}}
          rt (->Runtime host-env (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt user-id)]
      (-> (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
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

(defn hydrate-route-handler [->Runtime & {:keys [host-env path-params request-body user-id jwt]}]
  (try
    (let [local-basis (-> (:local-basis path-params) base-64-url-safe/decode read-edn-string) ; todo this can throw
          route (-> (:encoded-route path-params) base-64-url-safe/decode read-edn-string)
          _ (when-let [e (router/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) base-64-url-safe/decode read-edn-string)
          branch-aux (some-> (:branch-aux path-params) base-64-url-safe/decode read-edn-string)
          initial-state (-> {::runtime/user-id user-id
                             :stage request-body
                             ; should this be constructed with reducers?
                             ; why dont we need to preheat the tempid lookups here for parent branches?
                             ::runtime/partitions {branch {:local-basis local-basis
                                                           :route route
                                                           :hyperfiddle.runtime/branch-aux branch-aux}}})
          rt (->Runtime host-env (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt user-id)]
      (-> (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
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
