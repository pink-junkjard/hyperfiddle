(ns hypercrud.ui.native-event-listener
  (:require [reagent.core :as reagent]))


(if (= *target* "nodejs")
  ; todo this node implementation is wrong
  (def native-listener (fn [props child] child))
  (def native-listener (reagent/adapt-react-class js/NativeListener)))
