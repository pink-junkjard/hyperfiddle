(ns hypercrud.browser.pages.dump
  (:require [cljs.pprint :as pprint]
            [hypercrud.client.core :as hc]))


(defn ui [graph dbval dbid]
  [:div
   [:pre (with-out-str (pprint/pprint (hc/entity graph dbval dbid)))]])


(defn query [dbval dbid]
  {:dump ['[:find [?e ...] :in $ ?e :where [$ ?e]] [dbval (.-id dbid)] [dbval '[*]]]})
