(ns hypercrud.ui.input)


(defn input* [{:keys [value on-change] :as props}]
  (let [on-change #(let [newval (.. % -target -value)]
                    (on-change [value] [newval]))]
    [:input (assoc props :on-change on-change)]))
