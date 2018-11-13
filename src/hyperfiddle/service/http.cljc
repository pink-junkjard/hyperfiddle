(ns hyperfiddle.service.http
  (:require
    [cats.monad.either :as either]
    [contrib.reactive :as r]
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.router :as router]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.io.rpc-router :refer [decode-basis]]
    [hyperfiddle.reducers :as reducers]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime :refer [map->HostEnvironment]]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defmulti handle-route (fn [handler & rest] handler))

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
               :build BUILD
               :service-uri (->URI (str protocol "://" hostname)))
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

(defn local-basis-handler [->Runtime & {:keys [host-env route-params user-id jwt]}]
  (try
    (let [global-basis (decode-basis (:global-basis route-params)) ; todo this can throw
          route (router/decode (str "/" (:encoded-route route-params)))
          _ (either/branch
              (route/validate-route+ route)
              (fn [e] (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e)))
              (constantly nil))
          branch (some-> (:branch route-params) router/-decode-url-ednish)
          branch-aux (some-> (:branch-aux route-params) router/-decode-url-ednish)
          initial-state {::runtime/user-id user-id
                         ::runtime/global-basis global-basis
                         ::runtime/partitions {branch {:route route
                                                       ::runtime/branch-aux branch-aux}}}
          rt (->Runtime host-env (r/atom (reducers/root-reducer initial-state nil))
                        reducers/root-reducer jwt user-id)]
      ; todo should just call foundation/bootstrap-data
      (-> (actions/refresh-domain rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))
          (p/then (fn [_] (actions/refresh-user rt (partial runtime/dispatch! rt) #(deref (runtime/state rt)))))
          (p/then (fn [_] (runtime/local-basis rt branch)))
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

(defn hydrate-route-handler [->Runtime & {:keys [host-env route-params request-body user-id jwt]}]
  (try
    (let [local-basis (decode-basis (:local-basis route-params)) ; todo this can throw
          route (-> (str "/" (:encoded-route route-params)) router/decode)
          _ (either/branch
              (route/validate-route+ route)
              (fn [e] (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e)))
              (constantly nil))
          branch (some-> (:branch route-params) router/-decode-url-ednish)
          branch-aux (some-> (:branch-aux route-params) router/-decode-url-ednish)
          initial-state (reduce (fn [state [branch v]]
                                  (assoc-in state [::runtime/partitions branch :stage] v))
                                {::runtime/user-id user-id
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
          (p/then (fn [_] (runtime/hydrate-route rt branch)))
          (p/then (fn [data]
                    (let [cache-control (if (->> @(runtime/state rt [::runtime/partitions])
                                                 (map (comp :stage second))
                                                 (some seq))
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
