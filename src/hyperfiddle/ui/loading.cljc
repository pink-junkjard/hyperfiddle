(ns hyperfiddle.ui.loading
  (:require
    [hyperfiddle.domain :as domain]))


(defn- impl [img-src & children]
  [:div.loading-page
   [:div.logo-callout
    [:img {:src img-src}]
    #_[:span.brand ":hyperfiddle"]
    (when children
      (into [:div.additional-content] children))]])

(defn page [domain & children]
  (apply impl (domain/api-path-for domain :static-resource :resource-name "logo.png") children))
