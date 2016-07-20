(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [fieldinfo graph metatype forms value expanded-cur
                            change! transact! tempid!] :default))
