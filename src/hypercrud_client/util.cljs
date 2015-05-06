(ns hypercrud-client.util
  (:require [cljs.core.async :refer [<! >! chan close! map<]]))


(defn timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))
