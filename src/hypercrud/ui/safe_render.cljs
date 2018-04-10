(ns hypercrud.ui.safe-render
  (:require [contrib.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [reagent.core :as reagent]))


(code-for-nodejs
  (require '[reagent.dom.server :as reagent-server]))

(code-for-nodejs
  (defn- safe-reagent-call-impl [with-error user-fn & args]
    [:div.hyperfiddle-userportal
     {:dangerouslySetInnerHTML
      {:__html
       (try
         (reagent-server/render-to-string (apply vector user-fn args))
         (catch js/Error e
           (reagent-server/render-to-string [with-error e])))}}]))

(code-for-browser
  (defn- safe-reagent-call-impl [with-error user-fn & args]
    (let [e-state (reagent/atom nil)]
      (reagent/create-class
        {:reagent-render (fn [with-error user-fn & props]
                           [:div.hyperfiddle-userportal
                            (if-let [e @e-state]
                              [with-error e]
                              ; if we comment out "vector" it works with react-fragments
                              ; but not hiccup syntax, which is necessary to work with reagent-style
                              ; components with closure-constructor pattern.
                              (apply vector user-fn args))])

         :component-did-catch (fn [#_this e info]           ; args will need updating in reagent0.8.x
                                (reset! e-state e))}))))

(defn safe-reagent-call [with-error user-fn & args]
  (with-meta
    (into [safe-reagent-call-impl with-error user-fn] args)
    {:key (hash user-fn)}))

(defn foo [control & args]
  [:div.hyperfiddle-userportal
   (apply vector control args)])

(defn unify-portal-markup [control]
  (assert control)
  (reagent/partial foo control))
