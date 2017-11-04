(ns hypercrud.browser.context
  (:require [clojure.string :as string]
            [hypercrud.browser.auto-anchor-formula :as auto-anchor-formula]
            [hypercrud.browser.routing :as routing]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :keep-disabled-anchors?
          :route :query-params
          :relation :schemas
          :uri :find-element :schema
          :entity :attribute :value
          :layout :field))

(defn route [ctx route]
  (let [route (routing/tempid->id route ctx)
        initial-repository (->> (get-in ctx [:domain :domain/code-databases])
                                (filter #(= (:dbhole/name %) (:code-database route)))
                                first
                                (into {}))
        repository (let [overrides (->> (:query-params route)
                                        ; todo this is not sufficient for links on the page to inherit this override
                                        ; on navigate, this context is gone
                                        (filter (fn [[k _]] (and (string? k) (string/starts-with? k "$"))))
                                        (into {}))]
                     (update initial-repository :repository/environment merge overrides))]
    (-> ctx
        (update-in [:domain :domain/code-databases]
                   (fn [repos]
                     (map #(if (= initial-repository) repository %) repos)))
        (assoc :query-params (:query-params route)
               :route route
               :repository repository))))

(defn anchor-branch [ctx anchor]
  (if (:anchor/managed? anchor)
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (-> [(auto-anchor-formula/deterministic-ident ctx) (:db/id anchor)]
                           hash js/Math.abs - str)
          branch (branch/encode-branch-child (:branch ctx) child-id-str)]
      (assoc ctx :branch branch))
    ctx))

(defn relation [ctx relation]
  (assoc ctx :relation relation))

(defn user-with [ctx branch uri tx]
  ; todo custom user-dispatch with all the tx-fns as reducers
  ((:dispatch! ctx) (actions/with branch uri tx)))

(defn find-element [ctx fe]
  (let [uri (get-in ctx [:repository :repository/environment (:find-element/connection fe)])
        branch (:branch ctx)]
    (assoc ctx :uri uri
               :find-element fe
               :schema (get-in ctx [:schemas (:find-element/name fe)])
               :user-with! (reagent/partial user-with ctx branch uri))))

(defn entity [ctx entity]
  (assoc ctx :owner (if-let [owner-fn (:owner-fn ctx)]
                      (owner-fn entity ctx))
             :entity entity))

(defn attribute [ctx attribute]
  (assoc ctx :attribute (get-in ctx [:schema attribute])))

(defn value [ctx value]
  ; this is not reactive
  (assoc ctx :value value))
