(ns hypercrud.ui.select
  (:require [cats.core :refer-macros [mlet]]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.client.tx :as tx]
            [hypercrud.ui.options :as options]
            [hypercrud.ui.stale :as stale]
            [reagent.core :as reagent]))


(defn select-boolean* [value props ctx]
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             ((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) v)))
               :disabled (if (:read-only props) true false)}]
    [:select props
     [:option {:key true :value "true"} "True"]
     [:option {:key false :value "false"} "False"]
     [:option {:key :nil :value ""} "--"]]))

(let [on-change (fn [ctx e]
                  (let [id (options/options-e->id e)]
                    ((:user-with! ctx) (tx/update-entity-attr (:cell-data ctx) (:attribute ctx) id))))
      f (fn [value props ctx data']
          [stale/loading (mlet [{:keys [result ordered-fes ctx]} data'
                                options (options/options result ordered-fes ctx)
                                :let [props {:value (cond        ; normalize value for the dom
                                                      (nil? value) ""
                                                      :else (str (:db/id value)))
                                             :on-change (reagent/partial on-change ctx)
                                             :disabled (:read-only props)}]]
                           ; hack in the selected value if we don't have options hydrated?
                           ; Can't, since we only have the :db/id hydrated, and it gets complicated with relaton vs entity etc
                           [:select.select props options])
           (fn [e] (browser-ui/ui-error-inline e ctx))
           identity])]
  (defn select* [value options-anchor props ctx]
    [browser-ui/clickable-from-anchor options-anchor ctx
     (reagent/partial f value props ctx)]))
