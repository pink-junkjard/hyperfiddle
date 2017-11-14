(ns hypercrud.state.hydrating-action-batched                              ; browser
  (:require [clojure.set :as set]
            [hypercrud.client.http :as http]
            [hypercrud.state.core :as state]
            [promesa.core :as p]))

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))      ; WARNING copy pasted

; browser
(defn hydrating-action-batched [{:keys [on-start]} dispatch! get-state]
  (dispatch! (apply batch [:hydrate!-start (js/Math.random)] (if on-start (on-start get-state))))
  (let [{:keys [stage encoded-route] :as state} (get-state)]
    (-> (http/hydrate-route! #uri "/" encoded-route stage)
        (p/then (fn [{:keys [pulled-trees-map id->tempid]}]
                  (dispatch! [:set-ptm pulled-trees-map id->tempid])
                  (p/resolved nil)
                  #_(dispatch! [:batch
                                [:set-ptm pulled-trees-map id->tempid]
                                [:hydrate!-success]])))
        (p/then (fn [] (dispatch! [:hydrate!-success])
                  (p/resolved nil)))
        (p/catch #(dispatch! [:hydrate!-failure %]))))
  nil)
