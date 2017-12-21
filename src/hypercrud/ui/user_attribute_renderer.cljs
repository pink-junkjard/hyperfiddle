(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.anchor :as link]
            [hypercrud.compile.eval :as eval :refer [eval-str]]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.util.core :refer [pprint-str tee]]))


(defn user-attribute-renderer [ctx]
  (let [attr (:attribute ctx)]
    (or
      ; todo binding renderers should be pathed for aggregates and values
      #_((tee eval/validate-user-code-str
            #(if % (js/console.warn "using fiddle ctx/field renderer" (-> attr :db/ident str) %)))
        (get-in ctx [:fields (:db/ident attr) :renderer]))                                                   ; could be on the fiddle as well
      ((tee eval/validate-user-code-str
            #(if % (js/console.warn "using attribute/renderer " (-> attr :db/ident str) %)))
        (-> attr :attribute/renderer))))
  #_nil)

(defn user-attribute-render [ctx]
  (-> (if-let [user-fn-str (user-attribute-renderer ctx)]
        (eval-str user-fn-str)
        (either/left {:message "missing user-renderer"}))   ; double error check, remove this one
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [user-fn]
          (fn user-control [field links props ctx]
            (let [my-links (->> (link/links-lookup' links [(:fe-pos ctx) (-> ctx :attribute :db/ident)])
                                (filter :link/rel)          ; cannot lookup nil idents
                                (mapv (juxt #(-> % :link/rel) identity))
                                (into {}))
                  ctx (assoc ctx                            ; Todo unify link-fn with widget interface or something
                        :link-fn
                        (fn [ident label ctx]
                          (let [props (link/build-link-props (get my-links ident) ctx)] ; needs ctx to run formulas
                            [(:navigate-cmp ctx) props label])))]
              ; Same interface as auto-control widgets.
              ; pass value only as scope todo
              [safe-user-renderer user-fn field links props ctx]))))))
