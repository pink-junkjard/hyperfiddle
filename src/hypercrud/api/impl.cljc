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

(defn- needed-requests [request-fn id->tempid ptm]
  (let [requests (->> (request-fn id->tempid ptm) (into #{}))
        have-requests (set (keys ptm))
        new-requests (set/difference requests have-requests)]
    (when-not (set/subset? new-requests have-requests)
      (into [] new-requests))))

(defn- hydrate-loop-impl [rt request-fn local-basis stage id->tempid ptm total-loops]
  (if-let [new-requests (perf/time (fn [get-total-time] (timbre/debug "Computing needed requests" "total time: " (get-total-time)))
                                   (needed-requests request-fn id->tempid ptm))]
    (p/then (api/hydrate-requests rt local-basis stage new-requests)
            (fn [{:keys [pulled-trees id->tempid]}]
              (let [new-ptm (zipmap new-requests pulled-trees)
                    ptm (merge ptm new-ptm)]                ; todo this is a memory leak
                (hydrate-loop-impl rt request-fn local-basis stage id->tempid ptm (inc total-loops)))))
    (p/resolved {:id->tempid id->tempid
                 :ptm ptm
                 :total-loops total-loops})))

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
