(ns hypercrud.api.util
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [clojure.set :as set]
            [hypercrud.api.core :as api]
            [hypercrud.types.Err :refer [#?(:cljs Err)]]
            [hypercrud.util.performance :as perf]
            [promesa.core :as p]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Err Err))))


(defn- hydrate-loop-impl [rt request-fn local-basis stage id->tempid ptm total-loops]
  (let [all-requests (perf/time (fn [get-total-time] (timbre/debug "Computing needed requests" "total time: " (get-total-time)))
                                (->> (request-fn id->tempid ptm) (into #{})))
        missing-requests (let [have-requests (set (keys ptm))]
                           (->> (set/difference all-requests have-requests)
                                (into [])))]
    (if (empty? missing-requests)
      (p/resolved {:id->tempid id->tempid
                   :ptm (select-keys ptm all-requests)      ; prevent memory leak by returning exactly what is needed
                   :total-loops total-loops})
      (p/then (api/hydrate-requests rt local-basis stage missing-requests)
              (fn [{:keys [pulled-trees id->tempid]}]
                (let [new-ptm (zipmap missing-requests pulled-trees)
                      ptm (merge ptm new-ptm)]
                  (hydrate-loop-impl rt request-fn local-basis stage id->tempid ptm (inc total-loops))))))))

(defn hydrate-loop [rt request-fn local-basis stage id->tempid ptm]
  (let [hydrate-loop-id #?(:cljs (js/Math.random)
                           :clj  (Math/random))]
    (timbre/debug "Starting hydrate-loop" (str "[" hydrate-loop-id "]"))
    (-> (perf/time-promise (hydrate-loop-impl rt request-fn local-basis stage id->tempid ptm 0)
                           (fn [err get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time)))
                           (fn [success get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time) "total loops:" (:total-loops success))))
        (p/then #(dissoc % :total-loops)))))

(defn human-error [e req]
  (let [unfilled-holes (->> (filter (comp nil? val) (.-params req)) (map key))]
    ; what about EntityRequests? why are datomic errors not sufficient?
    (if-not (empty? unfilled-holes)
      {:message "Invalid query" :data {:datomic-error (.-msg e) :query (.-query req) :missing unfilled-holes}}
      {:message "Datomic error" :data {:datomic-error (.-msg e)}})))

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
