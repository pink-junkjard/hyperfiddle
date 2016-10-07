(ns hypercrud.browser.pages.dump
  (:require [cljs.pprint :as pprint]
            [hypercrud.client.core :as hc]))


(defn ui [graph id]
  [:div
   [:pre (with-out-str (pprint/pprint (hc/entity graph id)))]])


(defn query [id]
  {:dump ['[:find [?e ...] :in $ ?e :where [?e]] [id] '[*]]})
