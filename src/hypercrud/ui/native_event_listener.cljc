(ns hypercrud.ui.native-event-listener
  #?(:cljs (:require [reagent.core :as reagent])))


(def native-listener
  #?(:clj  (fn [& args] (assert false "todo"))
     :cljs (if (= *target* "nodejs")
             (fn [props child] child)                       ; todo this node implementation is wrong
             (reagent/adapt-react-class js/NativeListener))))
