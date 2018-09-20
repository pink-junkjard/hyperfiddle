(ns hyperfiddle.io.hydrate-route
  (:require
    [clojure.set :as set]
    [cuerdas.core :as str]
    [contrib.data :refer [map-keys]]
    [contrib.performance :as perf]
    [hypercrud.browser.router :as router]
    [hypercrud.client.peer :as peer]
    [hypercrud.types.EntityRequest :refer [#?(:cljs EntityRequest)]]
    [hypercrud.types.QueryRequest :refer [#?(:cljs QueryRequest)]]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.rpc-router :refer [encode-basis]]
    [hyperfiddle.reducers :as reducers]                     ; this import is immoral
    [hyperfiddle.runtime :as runtime]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (hypercrud.types.EntityRequest EntityRequest)
       (hypercrud.types.QueryRequest QueryRequest))))


(defn validate-user-qs [qs]
  {:pre [qs]
   :post [(not-any? nil? %)
          (every? #(or (instance? EntityRequest %) (instance? QueryRequest %)) %)]}
  (remove nil? qs) #_"userland convenience")

(def hydrate-loop-limit 25)

(defn- hydrate-loop-impl [rt request-fn local-basis branch stage {:keys [tempid-lookups ptm] :as data-cache} total-loops & [loop-limit]]
  (if (> total-loops loop-limit)
    (p/rejected (ex-info "Request limit reached" {:total-loops total-loops :ptm-at-cutoff (keys ptm)}))
    (let [all-requests (perf/time (fn [get-total-time] (timbre/debug "Computing needed requests" "total time: " (get-total-time)))
                                  (->> (request-fn tempid-lookups ptm)
                                       (validate-user-qs)
                                       (into #{})))
          missing-requests (let [have-requests (set (keys ptm))]
                             (->> (set/difference all-requests have-requests)
                                  (into [])))]
      (if (empty? missing-requests)
        (p/resolved {:tempid-lookups tempid-lookups
                     :ptm (select-keys ptm all-requests)    ; prevent memory leak by returning exactly what is needed
                     :total-loops total-loops})
        (p/then (runtime/hydrate-requests rt local-basis stage missing-requests)
                (fn [{:keys [pulled-trees] :as resp}]
                  (let [new-ptm (zipmap missing-requests pulled-trees)
                        ptm (merge ptm new-ptm)
                        data-cache {:tempid-lookups (merge-with merge tempid-lookups (get-in resp [:tempid-lookups branch]))
                                    :ptm ptm}]
                    (hydrate-loop-impl rt request-fn local-basis branch stage data-cache (inc total-loops) loop-limit))))))))

(defn hydrate-loop [rt request-fn local-basis branch stage & [data-cache]]
  (let [hydrate-loop-id #?(:cljs (js/Math.random)
                           :clj  (Math/random))]
    (timbre/debug "Starting hydrate-loop" (str "[" hydrate-loop-id "]"))
    (-> (perf/time-promise (hydrate-loop-impl rt request-fn local-basis branch stage data-cache 0 hydrate-loop-limit)
                           (fn [err get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time)))
                           (fn [success get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time) "total loops:" (:total-loops success))))
        (p/then #(dissoc % :total-loops)))))

(defn request-fn-adapter [local-basis route stage ctx ->Runtime f]
  ; Hacks because the hydrate-loop doesn't write to the state atom.
  (fn [tempid-lookups ptm]
    (let [ctx (update ctx :peer (fn [peer]
                                  (let [state (-> @(runtime/state peer)
                                                  ; want to keep all user/ui and bootstrapping state, just use overwrite the partition state.
                                                  (select-keys [::runtime/user-id ::runtime/domain])
                                                  (assoc-in [::runtime/partitions (:branch ctx)]
                                                            {:route route
                                                             ::runtime/branch-aux (::runtime/branch-aux ctx)
                                                             :local-basis local-basis
                                                             :ptm ptm
                                                             :tempid-lookups tempid-lookups}))
                                        state (reduce (fn [state [branch v]]
                                                        (assoc-in state [::runtime/partitions branch :stage] v))
                                                      state
                                                      stage)]
                                    (->Runtime (reducers/root-reducer state nil)))))]
      (f ctx))))

(defn hydrate-route-rpc! [service-uri local-basis route branch branch-aux stage & [jwt]]
  ; matrix params instead of path params
  (let [stage (->> stage
                   (remove (comp empty? second))
                   (into {}))]
    (-> (merge {:url (str/format "%(service-uri)shydrate-route/$local-basis/$branch/$branch-aux$encoded-route"
                                 {:service-uri service-uri
                                  :local-basis (encode-basis local-basis)
                                  :encoded-route (router/encode route) ; includes "/"
                                  :branch (router/-encode-pchar branch)
                                  :branch-aux (router/-encode-pchar branch-aux)})
                :accept :application/transit+json :as :auto}
               (when jwt {:auth {:bearer jwt}})
               (if (empty? stage)
                 {:method :get}                             ; Try to hit CDN
                 {:method :post
                  :form stage
                  :content-type :application/transit+json}))
        (http-request!)
        (p/then :body))))
