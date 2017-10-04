(ns hypercrud.ui.textarea)


(defn textarea* [{:keys [value on-change] :as props}]
  (let [on-change #(let [newval (.. % -target -value)]
                     (on-change [value] [newval]))]
    [:textarea (assoc props :on-change on-change)]))
