(ns hypercrud.platform.native-event-listener
  (:require [reagent.core :as reagent]))


(def native-listener (reagent/adapt-react-class js/NativeListener))
