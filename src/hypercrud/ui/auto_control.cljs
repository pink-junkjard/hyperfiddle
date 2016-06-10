(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [fieldinfo client forms value change! transact!] :default))
