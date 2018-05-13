(ns contrib.ui
  (:require
    [contrib.cljs-platform :refer [global!]]
    [reagent.core :as reagent]))


; Prevents failure in tests, this is omitted from test preamble
; We don't have a way to differentiate tests-node from runtime-node, so check presence
(def ^:export ReactSlickSlider (if-let [reactSlickSlider (aget (global!) "reactSlickSlider")]
                                 (reagent/adapt-react-class reactSlickSlider)))
