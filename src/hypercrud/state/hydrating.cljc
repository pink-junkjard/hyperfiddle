;(ns hypercrud.state.hydrating
;  (:require [cats.core :refer [mlet]]
;            [clojure.set :as set]
;            [hypercrud.api.http :as http-api]
;            [promesa.core :as p]))
;
;
;(defn hydrate-loop! [service-uri request-fn {:keys [local-basis ptm] :as state-val}] ; i belong in API
;  (let [requests (->> (request-fn state-val) (into #{}))
;        have-requests (set (keys ptm))
;        new-requests (set/difference requests have-requests)
;        new-requests-vec (into [] new-requests)]
;    ; inspect dbvals used in requests see if stage has changed for them
;    (if (set/subset? new-requests have-requests)
;      (p/resolved state-val)
;      (p/then (http-api/hydrate-requests! local-basis new-requests-vec (assert false "staged-branches"))
;              (fn [{:keys [pulled-trees id->tempid]}]
;                (let [ptm (zipmap new-requests-vec pulled-trees)]
;                  (assert false "process-results")
;                  (hydrate-loop! service-uri request-fn (assoc state-val
;                                                          :id->tempid id->tempid
;                                                          :ptm ptm))))))))

; force is a hack to be removed once this function runs in node
; TODO: this only runs in Node now, so it can be simplified
#_(defn hydrate-until-queries-settle!
    ([dispatch! get-state hydrate-id force]
     (hydrate-until-queries-settle! dispatch! get-state hydrate-id force state/*request* state/*local-basis*))
    ([dispatch! get-state hydrate-id force request-fn local-basis]
     (let [{:keys [ptm stage] :as state} (get-state)
           requests (->> (request-fn state) (into #{}))
           have-requests (set (keys ptm))
           new-requests (set/difference requests have-requests)
           new-requests-vec (into [] new-requests)]
       ; inspect dbvals used in requests see if stage has changed for them
       (if (or force (not (set/subset? new-requests have-requests)))
         (p/then (http-api/hydrate-requests! local-basis new-requests-vec (assert false "staged-branches"))
                 (fn [{:keys [pulled-trees id->tempid]}]
                   (assert false "process-results")
                   (when (= hydrate-id (:hydrate-id (get-state)))
                     (dispatch! [:set-ptm (zipmap new-requests-vec pulled-trees) id->tempid])
                     (hydrate-until-queries-settle! dispatch! get-state hydrate-id false request-fn local-basis))))
         (p/resolved nil)))))

; node
#_(defn hydrating-action-loop
    ([aux-actions dispatch! get-state]
     (hydrating-action-loop aux-actions dispatch! get-state false))
    ([{:keys [on-start]} dispatch! get-state force]
     (let [o-stage (:stage (get-state))
           hydrate-id (js/Math.random)                        ; todo want to hash state
           ]
       ; todo assert on-start is not a thunk
       (dispatch! (apply batch [:hydrate!-start hydrate-id] (if on-start (on-start get-state))))
       (-> (hydrate-until-queries-settle! dispatch! get-state hydrate-id
                                          (or force (not= o-stage (:stage (get-state)))))
           (p/then (fn []
                     (when (= hydrate-id (:hydrate-id (get-state)))
                       (dispatch! [:hydrate!-success]))))
           (p/catch (fn [error]
                      (when (= hydrate-id (:hydrate-id (get-state)))
                        (dispatch! [:hydrate!-failure error]))))))
     nil))
