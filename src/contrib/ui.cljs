(ns contrib.ui
  (:require
    [reagent.core :as reagent]))


; works on node?
(def ^:export ReactSlickSlider (reagent/adapt-react-class js/reactSlickSlider))
