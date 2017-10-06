(ns hypercrud.platform.safe-render
  (:require [reagent.dom.server :as reagent-server]))


(defn safe-user-renderer [user-fn & props]
  [:div.portal
   {:dangerouslySetInnerHTML
    {:__html
     (try
       (reagent-server/render-to-string (apply vector user-fn props))
       (catch js/Error e
         (reagent-server/render-to-string [:pre (pr-str e)])))}}])
