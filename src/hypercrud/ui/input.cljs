(ns hypercrud.ui.input
  (:require [reagent.core :as reagent]))


(defn input* [props]
  (let [props (update props :on-change (fn [on-change]
                                         #(on-change (.. % -target -value))))]
    [:input props]))


(defn validated-input [value set-attr! parse-string to-string valid?]
  (let [intermediate-val (reagent/atom (to-string value))]
    (fn [value set-attr! parse-string to-string valid?]
      [:input {:type "text"
               :class (if-not (valid? @intermediate-val) "invalid")
               :value @intermediate-val
               :on-change (fn [e]
                            (let [input-str (.. e -target -value)]
                              (reset! intermediate-val input-str) ;always save what they are typing
                              (if (valid? input-str)
                                (set-attr! (parse-string input-str)))))}])))
