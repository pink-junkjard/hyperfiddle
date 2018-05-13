(ns contrib.ui.remark
  (:require
    [contrib.data :refer [map-values]]
    [reagent.core :as reagent]))


(defn remark-render [remarkInstance value]
  (let [c (-> remarkInstance (.processSync value #js {"commonmark" true}) .-contents)
        content (-> c .-props .-children) #_"Throw away remark wrapper div"]
    content))

(defn remark [whitelist-reagent md-extension]
  (-> (js/remark)
      (.use js/remarkGenericExtensions
            (clj->js
              {"elements" (into {} (map vector (keys whitelist-reagent) (repeat {"html" {"properties" {"content" "::content::" "argument" "::argument::"}}})))}))
      (.use js/remarkReact
            (clj->js
              {"sanitize" false
               "remarkReactComponents" (->> whitelist-reagent
                                            (map-values
                                              (comp reagent/reactify-component md-extension)))}))))
