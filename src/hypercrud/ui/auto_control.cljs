(ns hypercrud.ui.auto-control)


(defmulti auto-control (fn [] :default))
(defmulti raw-control (fn [] :default))
(defmulti auto-table-cell (fn [] :default))
(defmulti raw-table-cell (fn [] :default))
(defmulti result (fn [] :default))
