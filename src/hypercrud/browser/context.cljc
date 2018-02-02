(ns hypercrud.browser.context
  (:require [hypercrud.browser.auto-anchor-formula :as auto-anchor-formula]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.branch :as branch]
            [hypercrud.util.core :as util]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation.actions :as foundation-actions]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
          :keep-disabled-anchors? :route
          :schemas :request :fiddle
          :uri :schema :user-with!
          :find-element :attribute :value
          :layout :field
          :cell :label                                      ; TODO :cell should cascade
          ))

(defn route [ctx route]
  {:pre [(:code-database route)
         (:hypercrud.browser/domain ctx)
         (seq (-> ctx :hypercrud.browser/domain :domain/code-databases))]
   :post [(-> % :hypercrud.browser/repository)
          (-> % :hypercrud.browser/repository :dbhole/uri)]}
  (assoc ctx :route (routing/tempid->id route ctx)
             :hypercrud.browser/repository (->> (get-in ctx [:hypercrud.browser/domain :domain/code-databases])
                                                (filter #(= (:dbhole/name %) (:code-database route)))
                                                first
                                                (into {}))))

(defn anchor-branch [ctx link]
  (if (:link/managed? link)
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (-> [(auto-anchor-formula/deterministic-ident ctx) (:db/id link)]
                           hash util/abs-normalized - str)
          branch (branch/encode-branch-child (:branch ctx) child-id-str)]
      (assoc ctx :branch branch))
    ctx))

(defn user-with [ctx branch uri tx]
  ; todo custom user-dispatch with all the tx-fns as reducers
  ((:dispatch! ctx) (foundation-actions/with (:peer ctx) branch uri tx)))

(defn relations [ctx relations]
  (assoc ctx :relations (reactive/atom relations)))

(defn relation [ctx relation]
  (assoc ctx :relation relation))

(defn find-element [ctx fe fe-pos]
  (-> (if-let [dbname (some-> (:source-symbol fe) str)]
        (let [uri (get-in ctx [:hypercrud.browser/repository :repository/environment dbname])]
          (assoc ctx :uri uri
                     :schema (get-in ctx [:schemas dbname])
                     :user-with! (reactive/partial user-with ctx (:branch ctx) uri)))
        ctx)

      ; todo why is fe necessary in the ctx?
      (assoc :find-element fe :fe-pos fe-pos)))

(let [f (fn [cell-data ctx]
          (if-let [owner-fn (:owner-fn ctx)]
            (owner-fn @cell-data ctx)))]
  (defn cell-data [ctx]
    (assert (:relation ctx))
    (assert (:fe-pos ctx))
    (let [cell-data (reactive/cursor (:relation ctx) [(:fe-pos ctx)])]
      (assoc ctx :owner (reactive/track f cell-data ctx)
                 :cell-data cell-data))))

(defn attribute [ctx attr-ident]
  (assoc ctx :attribute (get-in ctx [:schema attr-ident] {:db/ident attr-ident})))

(defn value [ctx value]
  (assoc ctx :value value))
