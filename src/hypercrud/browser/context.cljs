(ns hypercrud.browser.context
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]
            [reagent.core :as reagent]))


(defn clean [param-ctx]
  ; why not query-params and all the custom ui/render fns?
  (dissoc param-ctx
          :keep-disabled-anchors?
          :route :relation
          :schemas
          :conn-id :find-element :schema
          :entity :attribute :value
          :layout :field))

(defn route [param-ctx route]
  (assoc param-ctx :route route))

(defn anchor-branch [param-ctx anchor]
  (if (:anchor/managed? anchor)
    ; this auto-entity-dbid call makes no sense, there will be collisions, specifically on index links
    ; which means queries of unrendered modals are impacted, an unnecessary perf cost at the very least
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (:id (auto-entity-dbid param-ctx))
          branch (branch/encode-branch-child (:branch param-ctx) child-id-str)]
      (assoc param-ctx :branch branch))
    param-ctx))

(defn relation [param-ctx relation]
  (assoc param-ctx :relation relation))

(defn user-with [ctx branch conn-id tx]
  ; todo custom user-dispatch with all the tx-fns as reducers
  ((:dispatch! ctx) (actions/with branch conn-id tx)))

(defn find-element [param-ctx fe]
  (let [conn-id (-> fe :find-element/connection :db/id :id)
        branch (:branch param-ctx)]
    (assoc param-ctx :conn-id conn-id
                     :find-element fe
                     :schema (get-in param-ctx [:schemas (:find-element/name fe)])
                     :user-with! (reagent/partial user-with param-ctx branch conn-id))))

(defn entity [param-ctx entity]
  (assoc param-ctx :color (if-let [color-fn (:color-fn param-ctx)]
                            (color-fn entity param-ctx))
                   :owner (if-let [owner-fn (:owner-fn param-ctx)]
                            (owner-fn entity param-ctx))
                   :entity entity))

(defn attribute [param-ctx attribute]
  (assoc param-ctx :attribute (get-in param-ctx [:schema attribute])))

(defn value [param-ctx value]
  (assoc param-ctx :value value))
