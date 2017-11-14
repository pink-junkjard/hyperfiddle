(ns hypercrud.state.hydrating-action-loop                   ; node
  (:require [clojure.set :as set]
            [hypercrud.client.http :as http]
            [hypercrud.state.core :as state]
            [promesa.core :as p]))

; force is a hack to be removed once this function runs in node
; TODO: this only runs in Node now, so it can be simplified
(defn hydrate-until-queries-settle!
  ([dispatch! get-state hydrate-id force]
   (hydrate-until-queries-settle! dispatch! get-state hydrate-id force state/*request* state/*service-uri*))
  ([dispatch! get-state hydrate-id force request-fn service-uri]
   (js/console.log "...hydrate-until-queries-settle!; top")
   (let [{:keys [ptm stage] :as state} (get-state)
         ; if the time to compute the requests is long
         ; this peer could start returning inconsistent data compared to the state value,
         ; however another hydrating action should have dispatched in that scenario,
         ; so the resulting computation would be thrown away anyway
         requests (into #{} (request-fn state))]
     (js/console.log "...hydrate-until-queries-settle!; got requests " (count requests))
     ; inspect dbvals used in requests see if stage has changed for them
     (if (or force (not (set/subset? requests (set (keys ptm)))))
       (p/then (http/hydrate! service-uri requests stage)
               (fn [{:keys [pulled-trees id->tempid]}]
                 (js/console.log "...hydrate-until-queries-settle!; http! response")
                 (when (= hydrate-id (:hydrate-id (get-state)))
                   (js/console.log "...hydrate-until-queries-settle!; dispatching :ptm")
                   (dispatch! [:set-ptm (zipmap requests pulled-trees) id->tempid])
                   (js/console.log "...hydrate-until-queries-settle!; loop")
                   (hydrate-until-queries-settle! dispatch! get-state hydrate-id false request-fn service-uri))))
       (p/resolved nil)))))

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

; node
(defn hydrating-action-loop
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
                   (js/console.log "...hydrating-action; success ")
                   (when (= hydrate-id (:hydrate-id (get-state)))
                     (dispatch! [:hydrate!-success]))))
         (p/catch (fn [error]
                    (js/console.log "...hydrating-action; error= " error)
                    (when (= hydrate-id (:hydrate-id (get-state)))
                      (dispatch! [:hydrate!-failure error]))))))
   nil))
