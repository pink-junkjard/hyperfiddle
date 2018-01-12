(ns hyperfiddle.appfn.runtime-local
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [hypercrud.types.Err :refer [#?(:cljs Err)]]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hypercrud.util.performance :as perf]
            [hyperfiddle.api :as api]
            [promesa.core :as p]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Err Err))))


(def hydrate-loop-limit 25)

(defn- hydrate-loop-impl [rt request-fn local-basis stage {:keys [id->tempid ptm] :as data-cache} total-loops & [loop-limit]]
  (if (> total-loops loop-limit)
    (p/rejected (ex-info "Request limit reached" {:total-loops total-loops :ptm-at-cutoff (keys ptm)}))
    (let [all-requests (perf/time (fn [get-total-time] (timbre/debug "Computing needed requests" "total time: " (get-total-time)))
                                  (->> (request-fn id->tempid ptm) (into #{})))
          missing-requests (let [have-requests (set (map second (keys ptm)))]
                             (->> (set/difference all-requests have-requests)
                                  (into [])))]
      (if (empty? missing-requests)
        (p/resolved {:id->tempid id->tempid
                     :ptm (->> (select-keys (util/map-keys second ptm) all-requests)) ; prevent memory leak by returning exactly what is needed
                     :total-loops total-loops})
        (p/then (api/hydrate-requests rt local-basis stage missing-requests)
                (fn [{:keys [pulled-trees id->tempid]}]
                  (let [new-ptm (->> (zipmap missing-requests pulled-trees)
                                     (util/map-keys (fn [request]
                                                      [(branch/branch-vals-for-request request stage) request])))
                        ptm (merge ptm new-ptm)
                        data-cache {:id->tempid id->tempid
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

(defn human-error [e req]
  ; this is invalid on the jvm
  #_(let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
      ; what about EntityRequests? why are datomic errors not sufficient?
      (if-not (empty? unfilled-holes)
        {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}))
  (ex-info "Datomic error" {:datomic-error (.-msg e)}))

; this can be removed; #err can natively be Either
(defn process-result [resultset-or-error request]
  (if (instance? Err resultset-or-error)
    (either/left (human-error resultset-or-error request))
    (either/right resultset-or-error)))

(defn v-not-nil? [stmt]
  (or (map? stmt)
      (let [[op e a v] stmt]
        (not (and (or (= :db/add op) (= :db/retract op))
                  (nil? v))))))

(defn stage-val->staged-branches [stage-val]
  (->> stage-val
       (mapcat (fn [[branch-ident branch-content]]
                 (->> branch-content
                      (map (fn [[uri tx]]
                             {:branch-ident branch-ident
                              :uri uri
                              :tx (filter v-not-nil? tx)})))))))

(defn hydrate-one! [rt local-basis stage request]
  (-> (api/hydrate-requests rt local-basis stage [request])
      (p/then (fn [{:keys [pulled-trees]}]
                (-> (process-result (first pulled-trees) request)
                    (either/branch p/rejected p/resolved))))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [rt local-basis stage requests]
  (-> (api/hydrate-requests rt local-basis stage requests)
      (p/then (fn [{:keys [pulled-trees]}]
                (-> (map process-result pulled-trees requests)
                    (cats/sequence)
                    (either/branch p/rejected p/resolved))))))
