(ns hypercrud.state.hydrating-action-loop                   ; node
  (:require [clojure.set :as set]
            [hypercrud.client.upstream :as upstream]
            [hypercrud.state.core :as state]
            [promesa.core :as p]))

; force is a hack to be removed once this function runs in node
; TODO: this only runs in Node now, so it can be simplified
(defn hydrate-until-queries-settle!
  ([dispatch! get-state hydrate-id force]
   (hydrate-until-queries-settle! dispatch! get-state hydrate-id force state/*request* state/*service-uri* state/*local-basis*))
  ([dispatch! get-state hydrate-id force request-fn service-uri local-basis]
   (let [{:keys [ptm stage] :as state} (get-state)
         requests (->> (request-fn state) (into #{}))
         have-requests (set (keys ptm))
         new-requests (set/difference requests have-requests)
         new-requests-vec (into [] new-requests)]
     ; inspect dbvals used in requests see if stage has changed for them
     (if (or force (not (set/subset? new-requests have-requests)))
       (p/then (upstream/hydrate-requests! service-uri new-requests-vec local-basis stage)
               (fn [{:keys [pulled-trees id->tempid]}]
                 (when (= hydrate-id (:hydrate-id (get-state)))
                   (dispatch! [:set-ptm (zipmap new-requests-vec pulled-trees) id->tempid])
                   (hydrate-until-queries-settle! dispatch! get-state hydrate-id false request-fn service-uri local-basis))))
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
                   (when (= hydrate-id (:hydrate-id (get-state)))
                     (dispatch! [:hydrate!-success]))))
         (p/catch (fn [error]
                    (when (= hydrate-id (:hydrate-id (get-state)))
                      (dispatch! [:hydrate!-failure error]))))))
   nil))
