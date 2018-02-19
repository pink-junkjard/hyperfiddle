(ns hyperfiddle.ide.actions
  (:require [hyperfiddle.foundation.actions :refer [hydrate-partition refresh-partition-basis]]
            [promesa.core :as p]))


(defn set-user-profile [rt user-profile]
  (fn [dispatch! get-state]
    (when-not (= (:user-profile (get-state)) user-profile)
      (dispatch! [:set-user-profile user-profile])
      ; todo what about domain?
      (-> (refresh-partition-basis rt nil dispatch! get-state)
          (p/then (fn [] (hydrate-partition rt nil nil dispatch! get-state))))
      nil)))
