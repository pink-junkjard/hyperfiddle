(ns hyperfiddle.service.http
  (:require [contrib.reactive :as r]
            [hypercrud.browser.router :as router]
            [hypercrud.types.Err :refer [->Err]]
            [contrib.uri :refer [->URI]]
            [hyperfiddle.actions :as actions]
            [hyperfiddle.io.rpc-router :refer [decode-basis]]
            [hyperfiddle.reducers :as reducers]
            [hyperfiddle.runtime :as runtime :refer [map->HostEnvironment]]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


; hf.net cloud specific
(defn cloud-host-environment [{:keys [BUILD HF_HOSTNAMES HF_ALIAS_HOSTNAMES] :as env} protocol hostname]
  {:pre [hostname]}
  (let [hf-hostname (first (filter #(.endsWith hostname (str "." %)) HF_HOSTNAMES))
        hf-alias-hostname (first (filter #(.endsWith hostname (str "." %)) HF_ALIAS_HOSTNAMES))]
    (-> (cond
          hf-hostname (let [ident (second (re-find (re-pattern (str "(.*)\\." hf-hostname)) hostname))]
                        {:active-ide? (not= "www" ident)    ; should be true; hack "Because we can't fragment our domains"
                         :domain-eid [:domain/ident ident]
                         :auth/root hf-hostname
                         :ide/root hf-hostname})
          hf-alias-hostname {:active-ide? false
                             :domain-eid [:domain/ident (second (re-find (re-pattern (str "(.*)\\." hf-alias-hostname)) hostname))]
                             :auth/root hf-alias-hostname
                             :ide/root (first HF_ALIAS_HOSTNAMES)}
          :else-user-alias {:active-ide? false
                            :domain-eid [:domain/aliases hostname]
                            :auth/root nil
                            :ide/root (first HF_ALIAS_HOSTNAMES)})
        (assoc :hostname hostname
               :service-uri (->URI (str protocol "://" hostname "/api/" BUILD "/")))
        (map->HostEnvironment))))

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

(defn normalize-router-params [params]
  ; bide yields :0, pedestal yields :encoded-route
  (clojure.set/rename-keys params {:0 :encoded-route}))

(defn local-basis-handler [->Runtime & {:keys [host-env path-params user-id jwt]}]
  (try
    (let [path-params (normalize-router-params path-params)
          global-basis (decode-basis (:global-basis path-params)) ; todo this can throw
          route (router/decode (str "/" (:encoded-route path-params)))
          _ (when-let [e (router/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) router/-decode-url-ednish)
          branch-aux (some-> (:branch-aux path-params) router/-decode-url-ednish)
          initial-state {::runtime/user-id user-id
                         ::runtime/global-basis global-basis
                         ::runtime/partitions {branch {:route route
                                                       ::runtime/branch-aux branch-aux}}}
          rt (->Runtime host-env (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt user-id)]
      ; todo should just call foundation/bootstrap-data
      (-> (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then (fn [_] (actions/refresh-user rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))))
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
    (let [path-params (normalize-router-params path-params)
          local-basis (decode-basis (:local-basis path-params)) ; todo this can throw
          route (-> (str "/" (:encoded-route path-params)) router/decode)
          _ (when-let [e (router/invalid-route? route)] (throw e))
          branch (some-> (:branch path-params) router/-decode-url-ednish)
          branch-aux (some-> (:branch-aux path-params) router/-decode-url-ednish)
          initial-state (reduce (fn [state [branch v]]
                                  (assoc-in state [::runtime/partitions branch :stage] v))
                                {::runtime/user-id user-id
                                 :stage request-body
                                 ; should this be constructed with reducers?
                                 ; why dont we need to preheat the tempid lookups here for parent branches?
                                 ::runtime/partitions {branch {:local-basis local-basis
                                                               :route route
                                                               ::runtime/branch-aux branch-aux}}}
                                request-body)
          rt (->Runtime host-env (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt user-id)]
      ; todo should just call foundation/bootstrap-data
      (-> (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then (fn [_] (actions/refresh-user rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))))
          (p/then (fn [_] (runtime/hydrate-route rt local-basis route branch branch-aux request-body)))
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
