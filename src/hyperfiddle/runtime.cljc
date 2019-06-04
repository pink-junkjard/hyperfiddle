(ns hyperfiddle.runtime
  (:require
    [promesa.core :as p]))


(defprotocol HF-Runtime
  (domain [rt])
  (io [rt]))

(defprotocol State                                          ; internal
  (dispatch! [rt action-or-func])
  (state [rt] [rt path]))

(defn attribute-renderer [rt branch ident] @(state rt [::partitions branch :attr-renderers ident]))

(defn set-route
  "Set the route of the given branch. This may or may not trigger IO.
  Returns a promise"
  [rt branch route]
  (let [set-route (resolve 'hyperfiddle.actions/set-route)] ; todo intermediary hack for circular deps while we move away from dispatch! as the public api
    (p/promise
      (fn [resolve reject]
        (dispatch! rt (fn [dispatch! get-state]
                        (-> (set-route rt route branch false dispatch! get-state)
                            (p/branch resolve reject))))))))
