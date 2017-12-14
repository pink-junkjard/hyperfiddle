(ns hypercrud.ui.native-event-listener
  (:require [reagent.core :as reagent]))


(def native-listener
  (if (= *target* "nodejs")
    (fn [props child] child)                                ; todo this node implementation is wrong
    (reagent/adapt-react-class js/NativeListener)))
