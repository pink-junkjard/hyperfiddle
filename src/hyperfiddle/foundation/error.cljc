(ns hyperfiddle.foundation.error
  (:require [hypercrud.react.react-fragment :refer [react-fragment]]))


(defn error-cmp [e]
  [:div
   [:h1 "Fatal error"]
   [:fieldset [:legend "(pr-str e)"]
    [:pre (pr-str e)]]
   [:fieldset [:legend "(ex-data e)"]                       ; network error
    [:pre (pr-str (ex-data e))]]                            ; includes :body key
   [:fieldset [:legend "(.-stack e)"]                       ; network error
    [:pre (.-stack e)]]])


;(apply react-fragment :_)
;(markdown "### Helpful tip:
;* If Datomic error, check the staging area
;* If the staging area not visible, paste this at js console: `dispatch(hyperfiddle.main.root_ctx, hyperfiddle.foundation.state.actions.toggle_staging)`")
