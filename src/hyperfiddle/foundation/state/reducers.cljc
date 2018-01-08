(ns hyperfiddle.foundation.state.reducers
  (:require [hypercrud.state.core :as state]
            [hypercrud.state.reducers :as hc-reducers]))


(defn staging-open-reducer [staging-open? action & args]
  (case action
    :toggle-staging (not staging-open?)
    :hydrate!-failure true
    (or staging-open? false)))

(defn user-profile-reducer [user-profile action & args]
  (case action
    :set-user-profile (first args)
    user-profile))

(defn display-mode-reducer [display-mode action & args]
  (case action
    :toggle-display-mode (case display-mode
                           :xray :user
                           :user :xray)
    :set-display-mode (first args)
    (or display-mode :user)))

(def root-reducer-map
  (merge hc-reducers/root-reducer-map
         {:staging-open staging-open-reducer
          :user-profile user-profile-reducer
          :display-mode display-mode-reducer}))

(def root-reducer (state/combine-reducers root-reducer-map))
