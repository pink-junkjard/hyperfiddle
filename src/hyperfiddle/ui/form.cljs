(ns hyperfiddle.ui.form
  (:require
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.system-link :refer [system-link?]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hypercrud.browser.context :as context]))


(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/fmap :db/id (context/entity ctx)))]
    (if-not shadow-link (connection-color ctx))))

(defn ui-block-border-wrap [ctx class & children]
  [:div {:class class :style {:border-color (border-color ctx)}}
   (apply fragment children)])
