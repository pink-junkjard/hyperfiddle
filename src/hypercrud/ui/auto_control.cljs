(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [fieldinfo graph forms value expanded-cur
                            change! transact! tempid!] :default))
