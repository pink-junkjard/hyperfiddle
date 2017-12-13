(ns hypercrud.ui.input
  (:require [cats.monad.either :as either]
            [cljs.reader :as reader]
            [hypercrud.util.string :as hc-string]
            [reagent.core :as reagent]))


(defn read-string-or-nil [code-str]
  (either/branch (hc-string/safe-read-edn-string code-str)
                 (constantly nil)
                 identity))

(defn adapt-props-to-input [props]
  {:disabled (:read-only props)})

(defn- validated-input' [value on-change! parse-string to-string valid? props]
  (let [intermediate-val (reagent/atom (to-string value))]
    (fn [value on-change! parse-string to-string valid? props]
      ; todo this valid check should NOT be nil punning
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
  (let [parse-string read-string-or-nil
        to-string pr-str
        valid? #(let [value (read-string-or-nil %)]
                  (or (nil? value) (keyword? value)))]
    ^{:key value}
    [validated-input' value on-change! parse-string to-string valid? props]))

(defn edn-input* [value on-change! & [props]]
  (let [parse-string read-string-or-nil
        to-string pr-str
        valid? #(try (let [_ (reader/read-string %)]        ; differentiate between read `nil` and error
                       true)
                     (catch :default e false))]
    ^{:key value}
    [validated-input' value on-change! parse-string to-string valid? props]))

(defn id-input [value on-change! & [props]]
  ^{:key (:db/id value)}
  [validated-input' (:db/id value) on-change! read-string-or-nil pr-str
   read-string-or-nil
   props])
