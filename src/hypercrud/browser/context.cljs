(ns hypercrud.browser.context
  (:require [clojure.string :as string]
            [hypercrud.browser.auto-anchor-formula :as auto-anchor-formula]
            [hypercrud.browser.context-util :as context-util]
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
        ;_ (js/console.log (str (:debug ctx) (pr-str (:domain ctx))))
        respository (-> initial-repository
                        (update :source/databases
                                (fn [dbholes]
                                  (let [existing-db-map (util/group-by-assume-unique :dbhole/name dbholes)]
                                    (->> (:query-params ctx)
                                         ; todo this is not sufficient for links on the page to inherit this override
                                         ; on navigate, this context is gone
                                         (filter (fn [[k _]] (and (string? k) (string/starts-with? k "$"))))
                                         (map (fn [[k v]]
                                                [k {:dbhole/name k
                                                    :dbhole/uri v}]))
                                         (into {})
                                         (merge existing-db-map)
                                         (vals)
                                         (into #{}))))))]
    (-> ctx
        (update-in [:domain :domain/code-databases]
                   (fn [repos]
                     (map #(if (= initial-repository) respository %) repos)))
        (assoc :query-params (:query-params route)
               :route route
               :respository respository))))

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
  (let [uri (context-util/ident->database-uri (:find-element/connection fe) ctx)
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
