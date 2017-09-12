(ns hypercrud.ui.select
  (:require [cats.monad.either :as either]
            [hypercrud.browser.anchor :as anchor]
            [hypercrud.browser.browser-ui :as browser-ui]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.platform.native-event-listener :refer [native-listener]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.types.DbId :refer [->DbId]]))


(defn select-boolean* [value props param-ctx]
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (if (nil? value) "" (str value))
               ;; reconstruct the typed value
               :on-change #(let [v (case (.-target.value %)
                                     "" nil
                                     "true" true
                                     "false" false)]
                             ((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) v)))
               :disabled (if (:read-only props) true false)}]
    [:select props
     [:option {:key true :value "true"} "True"]
     [:option {:key false :value "false"} "False"]
     [:option {:key :nil :value ""} "--"]]))

(defn- select-options [options-anchor props param-ctx]
  (-> (option/hydrate-options' options-anchor param-ctx)
      (either/branch
        (fn [e] (browser-ui/ui-error e param-ctx))
        (fn [option-records]
          ; hack in the selected value if we don't have options hydrated?
          ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
          (let [no-options? (empty? option-records)
                props (update props :disabled #(or % no-options?))]
            [:select.select.ui props
             (concat
               (->> (sort-by second option-records)
                    (mapv (fn [[dbid label]]
                            ^{:key dbid}
                            [:option {:value (.-id dbid)} label])))
               [[:option {:key :blank :value ""} "--"]])])))))

(defn select* [value options-anchor props param-ctx]
  ; value :: {:db/id #DbId[17592186045891 17592186045422]}
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        :else (-> value :db/id :id str))

               ;; reconstruct the typed value
               :on-change #(let [select-value (.-target.value %)
                                 dbid (when (not= "" select-value)
                                        (let [id (js/parseInt select-value 10)
                                              id (if (< id 0) (str id) id)]
                                          (->DbId id (get-in param-ctx [:entity :db/id :conn-id]))))]
                             ((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) dbid)))
               :disabled (:read-only props)}
        props (if (#{:find-element/connection :dbhole/value :hypercrud/owner} (-> param-ctx :attribute :db/ident)) ; lol hack
                (assoc props :style {:background-color (connection-color/connection-color (-> value :db/id :id))})
                props)
        c #(if (contains? (:pressed-keys @(-> param-ctx :peer .-state-atom)) "alt")
             (do ((:dispatch! param-ctx) (actions/set-route (:route (anchor/build-anchor-props options-anchor param-ctx)))) (.stopPropagation %)))]
    [native-listener {:on-click c}
     [select-options options-anchor props param-ctx]]))
