(ns hypercrud.browser.context
  (:require [hypercrud.browser.routing :as routing]
            [hypercrud.util.reactive :as reactive]
            [hyperfiddle.foundation.actions :as foundation-actions]
            [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  ; why not code-database-uri and all the custom ui/render fns?
  (dissoc ctx
    :keep-disabled-anchors? :route
    :fiddle                                                 ; deprecated
    :fe-pos :uri :user-with!
    :value
    :layout :field
    :cell :label                                            ; TODO :cell should cascade

    :hypercrud.browser/attribute
    :hypercrud.browser/fat-attribute
    :hypercrud.browser/fiddle
    :hypercrud.browser/find-element
    :hypercrud.browser/links
    :hypercrud.browser/ordered-fes
    :hypercrud.browser/request
    :hypercrud.browser/result
    :hypercrud.browser/schema
    :hypercrud.browser/schemas))

(defn route [ctx route]
  {:pre [(if-let [params (:request-params route)] (vector? params) true) ; validate normalized already
         (some-> ctx :hypercrud.browser/domain :domain/fiddle-repo)]}
  (assoc ctx :route (routing/tempid->id route ctx)))

(letfn [(user-with [rt ctx branch uri tx]
          (runtime/dispatch! rt (foundation-actions/with rt (:hypercrud.browser/invert-route ctx) branch uri tx)))]
  (defn find-element [ctx fe fe-pos]
    (let [dbname (str @(reactive/cursor fe [:source-symbol]))
          uri (when dbname
                (get-in ctx [:hypercrud.browser/domain :domain/environment dbname]))
          user-with! (reactive/partial user-with (:peer ctx) ctx (:branch ctx) uri)]
      (assoc ctx
        :hypercrud.browser/find-element fe
        :hypercrud.browser/schema (reactive/cursor (:hypercrud.browser/schemas ctx) [dbname])
        :fe-pos fe-pos
        :uri uri
        :user-with! user-with!))))

(let [f (fn [cell-data ctx]
          (if-let [owner-fn (:owner-fn ctx)]
            (owner-fn @cell-data ctx)))]
  (defn cell-data [ctx relation]
    {:pre [(:fe-pos ctx)]}
    (let [cell-data (reactive/cursor relation [(:fe-pos ctx)])]
      (assoc ctx :owner (reactive/track f cell-data ctx)
                 :cell-data cell-data))))

(letfn [(default [default-v v] (or v default-v))]
  (defn attribute [ctx attr-ident]
    (let [fat-attr (->> (reactive/cursor (:hypercrud.browser/schema ctx) [attr-ident])
                        (reactive/fmap (reactive/partial default {:db/ident attr-ident})))]
      (assoc ctx
        :hypercrud.browser/attribute attr-ident
        :hypercrud.browser/fat-attribute fat-attr))))

(defn value [ctx value]
  {:pre [(reactive/reactive? value)]}
  (assoc ctx :value value))
