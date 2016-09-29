(ns hypercrud.browser.pages.dump
  (:require [cljs.pprint :as pprint]
            [hypercrud.client.core :as hc]
            [hypercrud.form.q-util :as q-util]))


(defn ui [graph id]
  [:div
   [:pre (with-out-str (pprint/pprint (hc/entity graph id)))]])


(defn query [id]
  (q-util/build-query :dump '[:find [?e ...] :in $ ?e :where [?e]] {"?e" id} '[*]))
