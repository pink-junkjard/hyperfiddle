(ns hypercrud.browser.context
  (:require [clojure.string :as string]
            [hypercrud.browser.auto-anchor-formula :as auto-anchor-formula]
            [hypercrud.browser.context-util :as context-util]
            [hypercrud.browser.routing :as routing]
            [hypercrud.state.actions.core :as actions]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [reagent.core :as reagent]))


(defn clean [param-ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc param-ctx
          :keep-disabled-anchors?
          :route :query-params
          :relation :schemas
          :uri :find-element :schema
          :entity :attribute :value
          :layout :field))

(defn override-domain-dbs [ctx]
  (update-in ctx
             [:domain :domain/databases]
             (fn [domain-dbs]
               (let [existing-db-map (util/group-by-assume-unique :dbhole/name domain-dbs)]
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
                      (into #{}))))))

(defn route [param-ctx route]
  (let [route (routing/tempid->id route param-ctx)]
    (assoc param-ctx
      :route route
      :query-params (:query-params route)
      :code-database-uri (->> (get-in param-ctx [:domain :domain/code-databases])
                              (filter #(= (:dbhole/name %) (:code-database route)))
                              first
                              :dbhole/uri))))

(defn anchor-branch [param-ctx anchor]
  (if (:anchor/managed? anchor)
    ; this auto-entity-dbid call makes no sense, there will be collisions, specifically on index links
    ; which means queries of unrendered modals are impacted, an unnecessary perf cost at the very least
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (auto-anchor-formula/deterministic-ident param-ctx)
          branch (branch/encode-branch-child (:branch param-ctx) child-id-str)]
      (assoc param-ctx :branch branch))
    param-ctx))

(defn relation [param-ctx relation]
  (assoc param-ctx :relation relation))

(defn user-with [ctx branch uri tx]
  ; todo custom user-dispatch with all the tx-fns as reducers
  ((:dispatch! ctx) (actions/with branch uri tx)))

(defn find-element [param-ctx fe]
  (let [uri (context-util/ident->database-uri (:find-element/connection fe) param-ctx)
        branch (:branch param-ctx)]
    (assoc param-ctx :uri uri
                     :find-element fe
                     :schema (get-in param-ctx [:schemas (:find-element/name fe)])
                     :user-with! (reagent/partial user-with param-ctx branch uri))))

(defn entity [param-ctx entity]
  (assoc param-ctx :owner (if-let [owner-fn (:owner-fn param-ctx)]
                            (owner-fn entity param-ctx))
                   :entity entity))

(defn attribute [param-ctx attribute]
  (assoc param-ctx :attribute (get-in param-ctx [:schema attribute])))

(defn value [param-ctx value]
  (assoc param-ctx :value value))
