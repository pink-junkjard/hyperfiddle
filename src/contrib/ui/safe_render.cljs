(ns contrib.ui.safe-render
  (:require [contrib.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [reagent.core :as reagent]))


(declare user-portal)

(code-for-nodejs
  (defn user-portal [with-error ?error-class & children]
    ; No portal in SSR, so errors will crash the whole page.
    ; IDE doesn't SSR so use the IDE to fix it.
    (into [:<>] children)))

(code-for-browser
  (defn user-portal [with-error e-props child]
    (let [show-error (atom false)
          e-state (reagent/atom nil)]
      (reagent/create-class
        {:reagent-render (fn [with-error e-props child]
                           (let [e @e-state]
                             (if (and @show-error e)
                               (do
                                 (reset! show-error false)  ; only show the error once, retry after that
                                 [with-error e e-props])
                               child)))

         :component-did-catch (fn [this e info]
                                (reset! show-error true)
                                (reset! e-state e))}))))
