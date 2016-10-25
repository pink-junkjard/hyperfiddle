(ns hypercrud.browser.pages.dump
  (:require [cljs.pprint :as pprint]))


(defn ui [entity]
  [:div
   [:pre (with-out-str (pprint/pprint entity))]])


(defn query [dbval dbid]
  {:dump ['[:find [?e ...] :in $ ?e :where [$ ?e]] [dbval (.-id dbid)] [dbval '[*]]]})
