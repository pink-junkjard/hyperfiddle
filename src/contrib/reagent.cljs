(ns contrib.reagent)


(defn ^:deprecated fragment [react-key & xs]
  (let [[k xs] (if (keyword? react-key)
                 [react-key xs]
                 [:_ (cons react-key xs)])]
    (into [:<> {:key (str k)}] xs)))
