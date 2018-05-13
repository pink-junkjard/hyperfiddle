(ns contrib.ui
  (:require
    [contrib.cljs-platform :refer [browser?]]
    [reagent.core :as reagent]))


; Prevents failure in tests, this is omitted from test preamble
(def ^:export ReactSlickSlider (if (browser?) (reagent/adapt-react-class js/reactSlickSlider)))
