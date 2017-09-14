(ns hypercrud.ui.select
  (:require [cats.monad.either :as either]
            [hypercrud.browser.core :as browser]
            [hypercrud.browser.connection-color :as connection-color]
            [hypercrud.client.tx :as tx]
            [hypercrud.form.option :as option]
            [hypercrud.types.DbId :refer [->DbId]]
            [reagent.core :as reagent]))


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

(defn select-anchor-renderer [props result ordered-fes anchors param-ctx]
  ; hack in the selected value if we don't have options hydrated?
  ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
  (let [no-options? (empty? result)
        props (-> props
                  (update :on-change (fn [on-change]
                                       (fn [e]
                                         (let [select-value (.-target.value e)
                                               id (when (not= "" select-value)
                                                    (let [id (js/parseInt select-value 10)]
                                                      (if (< id 0) (str id) id)))]
                                           (on-change id)))))
                  (update :disabled #(or % no-options?)))]
    [:select.select props
     (conj
       (->> result
            (mapv (fn [relation]
                    (let [entity (get relation (:find-element/name (first ordered-fes)))
                          label (-> (option/build-label ordered-fes relation param-ctx)
                                    ; It's perfectly possible to properly report this error properly upstream.
                                    (either/branch (fn [e] (pr-str e)) identity))]
                      [(:db/id entity) label])))
            (sort-by second)
            (map (fn [[dbid label]]
                   [:option {:key (str dbid) :value (.-id dbid)} label])))
       [:option {:key :blank :value ""} "--"])]))

(def always-user (atom :user))

(defn anchor->select [props anchor param-ctx]
  (let [renderer (reagent/partial select-anchor-renderer props)]
    [browser/ui anchor (assoc param-ctx
                         :display-mode always-user
                         :user-renderer renderer)]))

(defn select* [value options-anchor props param-ctx]
  ; value :: {:db/id #DbId[17592186045891 17592186045422]}
  (let [props {;; normalize value for the dom - value is either nil, an :ident (keyword), or eid
               :value (cond
                        (nil? value) ""
                        :else (-> value :db/id :id str))

               ;; reconstruct the typed value
               :on-change (fn [id]
                            (let [dbid (->DbId id (:conn-id param-ctx))]
                              ((:user-with! param-ctx) (tx/update-entity-attr (:entity param-ctx) (:attribute param-ctx) dbid))))
               :disabled (:read-only props)}
        props (if (#{:find-element/connection :dbhole/value :hypercrud/owner} (-> param-ctx :attribute :db/ident)) ; lol hack
                (assoc props :style {:background-color (connection-color/connection-color (-> value :db/id :id))})
                props)]
    [anchor->select props options-anchor param-ctx]))
