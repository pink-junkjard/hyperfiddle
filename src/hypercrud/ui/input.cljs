(ns hypercrud.ui.input
  (:require [reagent.core :as reagent]))


(defn- validated-input' [value on-change! parse-string to-string valid? props]
  (let [intermediate-val (reagent/atom (to-string value))]
    (fn [value on-change! parse-string to-string valid? props]
      (let [valid?' (valid? @intermediate-val)]
        [:input (merge props {:type "text"
                              :class (if-not valid?' "invalid")
                              :value @intermediate-val
                              :on-change #(reset! intermediate-val (.. % -target -value))
                              :on-blur #(let [parsed (parse-string @intermediate-val)]
                                         (if (and valid?' (not= parsed value))
                                           (on-change! parsed)))})]))))


(defn validated-input [value on-change! parse-string to-string valid? & [props]]
  ^{:key value}
  [validated-input' value on-change! parse-string to-string valid? props])


(defn input* [value on-change! & [props]]
  ^{:key value}
  [validated-input' value on-change! identity identity (constantly true) props])
