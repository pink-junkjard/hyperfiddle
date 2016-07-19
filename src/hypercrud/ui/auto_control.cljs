(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [fieldinfo graph metatype forms value change! transact! tempid!] :default))
