(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [field value args] :default))
