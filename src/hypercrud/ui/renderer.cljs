(ns hypercrud.ui.renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.anchor :as link]
            [hypercrud.compile.eval :as eval :refer [eval-str']]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.util.core :refer [pprint-str]]))


(defn user-cell-renderer [ctx]
  (let [attr (:attribute ctx)]
    (or
      ; todo binding renderers should be pathed for aggregates and values
      (eval/validate-user-code-str (get-in ctx [:fields (:db/ident attr) :renderer]))
      (eval/validate-user-code-str (-> attr :attribute/renderer)))))

(defn user-cell-render [maybe-field links props ctx]
  [:div.value
   (-> (if-let [user-fn-str (user-cell-renderer ctx)]
         (eval-str' user-fn-str)
         (either/left {:message "missing user-renderer"}))  ; double error check, remove this one
       (either/branch
         (fn [e] [:pre (pprint-str e)])
         (fn [user-fn]
           (let [my-links (->> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                               (filter :link/rel)           ; cannot lookup nil idents
                               (mapv (juxt #(-> % :link/rel) identity))
                               (into {}))
                 ctx (assoc ctx                             ; Todo unify link-fn with widget interface or something
                       :link-fn
                       (fn [ident label ctx]
                         (let [props (link/build-link-props (get my-links ident) ctx)] ; needs ctx to run formulas
                           [(:navigate-cmp ctx) props label])))]
             ; Same interface as auto-control widgets.
             ; pass value only as scope todo
             [safe-user-renderer user-fn maybe-field links props ctx]))))])
