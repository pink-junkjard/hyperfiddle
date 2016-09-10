(ns hypercrud.ui.input)


(defn input* [props]
  (let [props (update props :on-change (fn [on-change]
                                         #(on-change (.. % -target -value))))]
    [:input props]))
