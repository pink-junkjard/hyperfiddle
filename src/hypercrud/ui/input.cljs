(ns hypercrud.ui.input
  (:require [cljs.reader :as reader]
            [hypercrud.form.q-util :as q-util]
            [reagent.core :as reagent]))


(defn adapt-props-to-input [props]
  (select-keys props [:read-only]))


(defn- validated-input' [value on-change! parse-string to-string valid? props]
  (let [intermediate-val (reagent/atom (to-string value))]
    (fn [value on-change! parse-string to-string valid? props]
      (let [valid?' (valid? @intermediate-val)]
        [:input (merge (adapt-props-to-input props)
                       {:type "text"
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


(defn keyword-input* [value on-change! & [props]]
  (let [parse-string q-util/safe-read-string
        to-string pr-str
        valid? #(try (let [value (reader/read-string %)]
                       (or (nil? value) (keyword? value)))
                     (catch :default e false))]
    ^{:key value}
    [validated-input' value on-change! parse-string to-string valid? props]))


(defn edn-input* [value on-change! & [props]]
  (let [parse-string q-util/safe-read-string
        to-string pr-str
        valid? #(try (let [_ (reader/read-string %)]
                       true)
                     (catch :default e false))]
    ^{:key value}
    [validated-input' value on-change! parse-string to-string valid? props]))
