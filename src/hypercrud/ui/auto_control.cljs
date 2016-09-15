(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [] :default))

(defmulti auto-table-cell (fn [] :default))
