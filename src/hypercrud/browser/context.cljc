(ns hypercrud.browser.context
  (:require [cats.monad.either :refer [branch]]
            [hypercrud.browser.routing :as routing]
            [hypercrud.util.reactive :as reactive]
            [hypercrud.util.string :refer [memoized-safe-read-edn-string]]
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

(defn relations [ctx rv]
  {:pre [(reactive/reactive? rv)]}
  (assoc ctx :relations rv))

(defn relation [ctx rv]
  {:pre [(reactive/reactive? rv)]}
  ; (assoc ctx :relation @(reactive/cursor (:relations ctx) [i]))
  ; Break the pattern - :relations is not in scope in form case which is a bit of information.
  (assoc ctx :relation rv))

(letfn [(user-with [rt ctx branch uri tx]
          (runtime/dispatch! rt (foundation-actions/with rt (:hypercrud.browser/domain ctx) branch uri tx)))]
  (defn find-element [ctx fe-pos]
    (let [fe (reactive/cursor (:hypercrud.browser/ordered-fes ctx) [fe-pos])
          dbname (str @(reactive/cursor fe [:source-symbol]))
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
  (defn cell-data [ctx]                                     ; "dependent"
    {:pre [(:fe-pos ctx)]}
    (let [cell-data (reactive/cursor (:relation ctx) [(:fe-pos ctx)])]
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

(letfn [(get-value-f [attr fields]
          (->> fields
               (filter #(= (:attribute %) attr))
               first
               :cell-data->value))]
  (defn relation-path [ctx link]
    ; opposite of link/same-path-as
    (branch
      (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      #(throw %)
      (fn [[fe-pos attr]]
        (as-> ctx $
              ;(with-relations)                                    ; already here
              ;(relation (reactive/atom [domain]))                 ; already here
              (find-element $ fe-pos)
              (if (:link/dependent? link) (cell-data $) $)
              (if attr (attribute $ attr) $)
              (if (and (:link/dependent? link) attr)
                (let [f (->> (reactive/cursor (:hypercrud.browser/find-element $) [:fields])
                             (reactive/fmap (reactive/partial get-value-f attr)))]
                  (value $ (reactive/fmap f (:cell-data $))))
                $))))))
