(ns hyperfiddle.io.hydrate-route
  (:require [clojure.set :as set]
            [cuerdas.core :as str]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.types.EntityRequest :refer [#?(:cljs EntityRequest)]]
            [hypercrud.types.QueryRequest :refer [#?(:cljs QueryRequest)]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hypercrud.util.performance :as perf]
            [hyperfiddle.appval.state.reducers :as reducers]
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

(defn- hydrate-loop-impl [rt request-fn local-basis stage {:keys [id->tempid ptm] :as data-cache} total-loops & [loop-limit]]
  (if (> total-loops loop-limit)
    (p/rejected (ex-info "Request limit reached" {:total-loops total-loops :ptm-at-cutoff (keys ptm)}))
    (let [all-requests (perf/time (fn [get-total-time] (timbre/debug "Computing needed requests" "total time: " (get-total-time)))
                                  (->> (request-fn id->tempid ptm)
                                       (validate-user-qs)
                                       (into #{})))
          missing-requests (let [have-requests (set (map second (keys ptm)))]
                             (->> (set/difference all-requests have-requests)
                                  (into [])))]
      (if (empty? missing-requests)
        (p/resolved {:id->tempid id->tempid
                     :ptm (->> (select-keys (util/map-keys second ptm) all-requests)) ; prevent memory leak by returning exactly what is needed
                     :total-loops total-loops})
        (p/then (runtime/hydrate-requests rt local-basis stage missing-requests)
                (fn [{:keys [pulled-trees] :as resp}]
                  (let [new-ptm (->> (zipmap missing-requests pulled-trees)
                                     (util/map-keys (fn [request]
                                                      [(branch/branch-vals-for-request request stage) request])))
                        ptm (merge ptm new-ptm)
                        data-cache {:id->tempid (merge-with #(merge-with merge %1 %2) id->tempid (:id->tempid resp))
                                    :ptm ptm}]
                    (hydrate-loop-impl rt request-fn local-basis stage data-cache (inc total-loops) loop-limit))))))))

(defn hydrate-loop [rt request-fn local-basis stage & [data-cache]]
  (let [hydrate-loop-id #?(:cljs (js/Math.random)
                           :clj  (Math/random))]
    (timbre/debug "Starting hydrate-loop" (str "[" hydrate-loop-id "]"))
    (-> (perf/time-promise (hydrate-loop-impl rt request-fn local-basis stage data-cache 0 hydrate-loop-limit)
                           (fn [err get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time)))
                           (fn [success get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time) "total loops:" (:total-loops success))))
        (p/then #(dissoc % :total-loops)))))

(defn hydrate-loop-adapter [local-basis stage ctx ->Runtime f]
  ; Hacks because the hydrate-loop doesn't write to the state atom.
  (fn [id->tempid ptm]
    (let [state-val {:local-basis local-basis
                     :tempid-lookups id->tempid
                     :ptm ptm
                     :stage stage}
          ctx (assoc ctx :peer (->Runtime (reducers/root-reducer state-val nil)))]
      (f ctx))))

(defn hydrate-route-rpc! [service-uri local-basis encoded-route foo target-repo branch stage]
  (-> (merge {:url (str/format "%(service-uri)shydrate-route/$local-basis/$double-encoded-route/$foo/$target-repo/$branch"
                               {:service-uri service-uri
                                :local-basis (base-64-url-safe/encode (pr-str local-basis))
                                :double-encoded-route (base-64-url-safe/encode encoded-route) ; todo this is awful
                                :foo (base-64-url-safe/encode (pr-str foo))
                                :target-repo (base-64-url-safe/encode (pr-str target-repo))
                                :branch (base-64-url-safe/encode (pr-str branch))})
              :accept :application/transit+json :as :auto}
             (if (empty? stage)
               {:method :get}                               ; Try to hit CDN
               {:method :post
                :form stage
                :content-type :application/transit+json}))
      (http-request!)
      (p/then :body)))
