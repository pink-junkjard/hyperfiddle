(ns hypercrud.ui.safe-render
  (:require [hypercrud.util.cljs-platform :refer [code-for-browser code-for-nodejs]]
            [reagent.core :as reagent]))


(declare safe-user-renderer)

(code-for-nodejs
  (require '[reagent.dom.server :as reagent-server]))

(code-for-nodejs
  (defn safe-user-renderer [user-fn & props]
    [:div.portal
     {:dangerouslySetInnerHTML
      {:__html
       (try
         (reagent-server/render-to-string (apply vector user-fn props))
         (catch js/Error e
           (reagent-server/render-to-string [:pre (pr-str e)])))}}]))

(code-for-browser
  (defn safe-render! [this]
    (let [[react-ctor user-fn & props] (reagent/argv this)
          dom-el (reagent/dom-node this)]
      (try
        ; construct hiccup markup for reagent lolz
        (reagent/render (with-meta (apply vector user-fn props) {:key (hash user-fn)}) dom-el)
        (catch js/Error e
          (reagent/render [:pre (pr-str e)] dom-el)))))

  (def safe-user-renderer
    (reagent/create-class
      {:reagent-render (fn [user-fn & props] [:div.portal]) ; portal container
       :component-did-mount safe-render!
       :component-will-unmount
       (fn [this]
         (reagent/unmount-component-at-node (reagent/dom-node this)))
       :component-did-update safe-render!})))
