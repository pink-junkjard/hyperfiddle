(ns hypercrud.platform.safe-render
  (:require [reagent.core :as reagent]))


(defn safe-user-renderer [user-fn & props]
  [:div.portal
   {:dangerouslySetInnerHTML
    {:__html
     (try
       (reagent/render-to-string (apply vector user-fn props))
       (catch js/Error e
         (reagent/render-to-string [:pre (pr-str e)])))}}])
