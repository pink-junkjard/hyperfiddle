(ns hypercrud.ui.input)


(defn input* [{:keys [value on-change] :as props}]
  (let [on-change #(let [newval (.. % -target -value)]
                    (on-change [:db/retract value]
                               [:db/add newval]))]
    [:input (assoc props :on-change on-change)]))
