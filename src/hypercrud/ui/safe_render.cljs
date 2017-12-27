(ns hypercrud.ui.safe-render
  (:require [hypercrud.util.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [reagent.core :as reagent]))


(declare safe-user-renderer)

(code-for-nodejs
  (require '[reagent.dom.server :as reagent-server]))

; Two levels of containers. You need a real dom-el for child react to attach to.
; Making the inner react-root explicit means user-fn can be react fragments.
; Which abstracts the codebase from caring if we are in a portal or not.

(code-for-nodejs
  (defn safe-user-renderer [user-fn & props]
    (let [user-result (apply vector user-fn props)
          explicit-container [:div.hyperfiddle-userroot {:key (hash user-fn)}
                              user-result]]
      [:div.hyperfiddle-userportal
       {:dangerouslySetInnerHTML
        {:__html
         (try
           (reagent-server/render-to-string explicit-container)
           (catch js/Error e
             (reagent-server/render-to-string [:pre (pr-str e)])))}}])))

(code-for-browser
  (defn safe-render! [this]
    (let [[react-ctor user-fn & props] (reagent/argv this)
          dom-el (reagent/dom-node this)

          ; if we comment out "vector" it works with react-fragments
          ; but not hiccup syntax, which is necessary to work with reagent-style
          ; components with closure-constructor pattern.
          user-result (apply vector user-fn props)
          explicit-container [:div.hyperfiddle-userroot {:key (hash user-fn)}
                              user-result]]
      (try
        (reagent/render explicit-container dom-el)
        (catch js/Error e
          (reagent/render [:pre (pr-str e)] dom-el)))))

  (def safe-user-renderer
    (reagent/create-class
      {:reagent-render (fn [user-fn & props] [:div.hyperfiddle-userportal])
       :component-did-mount safe-render!
       :component-will-unmount
       (fn [this]
         (reagent/unmount-component-at-node (reagent/dom-node this)))
       :component-did-update safe-render!})))

(defn foo [control & args]
  [:div.hyperfiddle-userportal
   [:div.hyperfiddle-userroot
    (apply vector control args)]])

(defn unify-portal-markup [control]
  (assert control)
  (reagent/partial foo control))
