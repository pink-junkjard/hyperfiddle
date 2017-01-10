(ns hypercrud.client.util
  (:require [hypercrud.types :refer [->DbId]]))


(defn temp-id!-factory [temp-id-atom]
  (fn [conn-id]
    (let [updated-value (swap! temp-id-atom update conn-id dec)
          id (get updated-value conn-id)]
      (->DbId id conn-id))))
