(ns hyperfiddle.foundation.state.actions
  (:require [hypercrud.state.actions.core :refer [refresh-page-local-basis hydrate-page]]
            [promesa.core :as p]))


(defn toggle-staging [] [:toggle-staging])

(defn set-user-profile [rt user-profile]
  (fn [dispatch! get-state]
    (when-not (= (:user-profile (get-state)) user-profile)
      (dispatch! [:set-user-profile user-profile])
      (-> (refresh-page-local-basis rt dispatch! get-state)
          (p/then (fn [] (hydrate-page rt nil dispatch! get-state))))
      nil)))

(defn set-display-mode [display-mode]
  [:set-display-mode display-mode])
