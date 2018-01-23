(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.link :as link]
            [hypercrud.compile.eval :as eval]
            [hypercrud.ui.safe-render :refer [safe-user-renderer]]
            [hypercrud.util.core :refer [pprint-str]]))


(defn eval-user-control-ui [s]
  (-> (eval/eval-str s)
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
