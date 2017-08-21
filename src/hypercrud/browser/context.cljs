(ns hypercrud.browser.context
  (:require [hypercrud.client.core :as hc]
            [hypercrud.browser.auto-anchor-formula :refer [auto-entity-dbid]]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]))


(defn clean [param-ctx]
  ; why not query-params and all the custom ui/render fns?
  (dissoc param-ctx
          :route :result
          :db :find-element :schema
          :entity :attribute :value
          :layout :field))

(defn route [param-ctx route]
  (assoc param-ctx :route route))

(defn anchor-branch [param-ctx anchor]
  (if (:anchor/managed? anchor)
    (if-let [db (:db param-ctx)]
      (let [branch (branch/encode-branch-child (.-branch db) (:id (auto-entity-dbid param-ctx)))]
        (-> param-ctx
            ; if we are an index link, what are we forking? Provide a binding
            (assoc-in [:branches (.-conn-id db)] branch)
            (update :db #(hc/db (:peer param-ctx) (.-conn-id %) branch))))
      (do
        (js/console.warn "You are attempting to branch an index-link. We can't deduce the :db to branch, you must explicitly set it in user bindings.")
        param-ctx))
    param-ctx))

; todo :result -> :relation
(defn relation [param-ctx relation]
  (assoc param-ctx :result relation))

(defn find-element [param-ctx db fe]
  (assoc param-ctx :db db
                   :find-element fe
                   ; todo custom user-dispatch with all the tx-fns as reducers
                   :user-with! (fn [tx] ((:dispatch! param-ctx) (actions/with (.-conn-id db) (.-branch db) tx)))))

(defn entity [param-ctx entity]
  (assoc param-ctx :color ((:color-fn param-ctx) entity param-ctx)
                   :owner ((:owner-fn param-ctx) entity param-ctx)
                   :entity entity))

(defn attribute [param-ctx attribute]
  (assoc param-ctx :attribute attribute))

(defn value [param-ctx value]
  (assoc param-ctx :value value))
