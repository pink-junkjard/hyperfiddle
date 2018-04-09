(ns hypercrud.ui.safe-render
  (:require [contrib.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [reagent.core :as reagent]))


(declare safe-reagent-call)

(code-for-nodejs
  (require '[reagent.dom.server :as reagent-server]))

; Two levels of containers. You need a real dom-el for child react to attach to.
; Making the inner react-root explicit means user-fn can be react fragments.
; Which abstracts the codebase from caring if we are in a portal or not.

(code-for-nodejs
  (defn safe-reagent-call [with-error user-fn & props]
    [:div.hyperfiddle-userportal
     {:dangerouslySetInnerHTML
      {:__html
       (try
         (reagent-server/render-to-string (apply vector user-fn props))
         (catch js/Error e
           (reagent-server/render-to-string [with-error e])))}}]))

(code-for-browser
  (defn safe-reagent-call [with-error user-fn & props]
    (let [e-state (reagent/atom nil)]
      (reagent/create-class
        {:reagent-render (fn [with-error user-fn & props]
                           (if-let [e @e-state]
                             [with-error e]
                             [:div.hyperfiddle-userportal
                              ; if we comment out "vector" it works with react-fragments
                              ; but not hiccup syntax, which is necessary to work with reagent-style
                              ; components with closure-constructor pattern.
                              (apply vector user-fn props)]))

         :component-did-catch (fn [#_this e info]           ; args will need updating in reagent0.8.x
                                (reset! e-state e))}))))

(defn foo [control & args]
  [:div.hyperfiddle-userportal
   (apply vector control args)])

(defn unify-portal-markup [control]
  (assert control)
  (reagent/partial foo control))
