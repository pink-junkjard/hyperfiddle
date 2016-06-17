(ns hypercrud.client.reagent-browser
  (:require [hypercrud.client.reagent :as hcr]
            [reagent.core :as reagent]))


(defmethod hcr/force-update! :default [cmp comp-fn]
  (reagent/force-update cmp))
