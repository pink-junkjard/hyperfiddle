(ns hypercrud.state.actions.internal
  (:require [promesa.core :as p]
            [hypercrud.client.http :as http]
            [clojure.set :as set]))

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

; force is a hack to be removed once this function runs in node
(defn hydrate-until-queries-settle! [dispatch! get-state peer hydrate-id force]
  (let [{:keys [entry-uri ptm stage] :as state} (get-state)
        ; if the time to compute the requests is long
        ; this peer could start returning inconsistent data compared to the state value,
        ; however another hydrating action should have dispatched in that scenario,
        ; so the resulting computation would be thrown away anyway
        ;
        ; re: circular dep
        ; technically request should never fire an action, so if request was separated from ui, no circle in theory
        requests (into #{} (hyperfiddle.app/request state peer))
        hydrated? (set/subset? (set requests) (set (keys ptm)))]
    ; inspect dbvals used in requests see if stage has changed for them
    (if (or force (not hydrated?))
      (p/then (http/hydrate! entry-uri requests stage)
              (fn [{:keys [t pulled-trees-map]}]
                (when (= hydrate-id (:hydrate-id (get-state)))
                  (dispatch! [:set-ptm pulled-trees-map])
                  (hydrate-until-queries-settle! dispatch! get-state peer hydrate-id false))))
      (p/resolved nil))))
