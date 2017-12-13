(ns hypercrud.api.impl
  (:require [clojure.set :as set]
            [hypercrud.api.core :as api]
            [hypercrud.util.performance :as perf]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


;(defn global-basis [])
;
;(defn local-basis! [global-basis route state-val])
;
;(defn hydrate-route! [local-basis route state-val])
;
;(defn edge [route])
;
;; has datomic dep
;#_(defn hydrate-requests [local-basis requests staged-branches])
;
;; has datomic dep
;#_(defn transact! [tx-groups])
;
;; has datomic dep
;#_(defn sync [dbs])

(defn global-basis [rt]
  (p/resolved nil))

(defn local-basis [rt global-basis]
  (p/resolved global-basis))

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
                           :clj  (assert false "todo"))]
    (timbre/debug "Starting hydrate-loop" (str "[" hydrate-loop-id "]"))
    (-> (perf/time-promise (hydrate-loop-impl rt request-fn local-basis stage id->tempid ptm 0)
                           (fn [err get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time)))
                           (fn [success get-total-time]
                             (timbre/debug "Finished hydrate-loop" (str "[" hydrate-loop-id "]") "total time:" (get-total-time) "total loops:" (:total-loops success))))
        (p/then #(dissoc % :total-loops)))))
