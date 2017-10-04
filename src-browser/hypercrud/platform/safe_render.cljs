(ns hypercrud.platform.safe-render
  (:require [reagent.core :as reagent]))


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
    {:reagent-render (fn [user-fn & props] [:div.portal])   ; portal container
     :component-did-mount safe-render!
     :component-will-unmount
     (fn [this]
       (reagent/unmount-component-at-node (reagent/dom-node this)))
     :component-did-update safe-render!}))
