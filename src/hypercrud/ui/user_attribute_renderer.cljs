(ns hypercrud.ui.user-attribute-renderer
  (:require [cats.monad.either :as either]
            [hypercrud.browser.link :as link]
            [hypercrud.compile.eval :as eval]
            [hypercrud.ui.safe-render :refer [safe-reagent-call]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.util.reactive :as reactive]))


(defn- lookup-link [ctx rel]
  (->> @(:hypercrud.brower/links ctx)
       (filter #(= (:link/rel %) rel))
       (map (fn [link]
              ; backwards compat, why is this a thing for custom user controls?
              (if (link/popover-link? link)
                (assoc link :link/render-inline? false)
                link)))
       (filter (link/same-path-as? [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]))
       first))

(defn safe-eval-user-control-fn [s]
  (-> (eval/eval-str s)
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [f]
          (when f
            (fn user-control [field props ctx]
              (let [lookup-link (reactive/partial lookup-link ctx)
                    ctx (assoc ctx                          ; Todo unify link-fn with widget interface or something
                          :link-fn
                          (fn [ident label ctx]
                            (let [link @(reactive/track lookup-link ident)
                                  props (link/build-link-props link ctx)] ; needs ctx to run formulas
                              [(:navigate-cmp ctx) props label])))]
                ; Same interface as auto-control widgets.
                ; pass value only as scope todo
                [safe-reagent-call f field props ctx])))))))

(defn safe-eval-user-expr [s]
  (-> (eval/eval-str s)
      (either/branch
        (fn l [e] [:pre (pprint-str e)])
        (fn r [v] v))))
