(ns contrib.ui.input
  (:require
    [cats.monad.either :as either]
    [clojure.set :refer [rename-keys]]
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.string :refer [safe-read-edn-string]]))


(defn adapt-props [props]
  (rename-keys props {:read-only :disabled}))

(defn read-string-or-nil [code-str]
  (either/branch (safe-read-edn-string code-str)
                 (constantly nil)
                 identity))

(defn- validated-input' [value on-change! parse-string to-string valid? props]
  (let [intermediate-val (r/atom (to-string value))]
    (fn [value on-change! parse-string to-string valid? props]
      ; todo this valid check should NOT be nil punning
      (let [valid?' (valid? @intermediate-val)]
        [:input (merge (adapt-props props)
                       {:type "text"
                        :class (css (if-not valid?' "invalid") (:class props))
                        :value @intermediate-val
                        :on-change #(reset! intermediate-val (.. % -target -value))
                        :on-blur #(let [parsed (parse-string @intermediate-val)]
                                    (if (and valid?' (not= parsed value))
                                      (on-change! parsed)))})]))))

(defn validated-input [value on-change! parse-string to-string valid? & [props]]
  ^{:key value}                                             ; This is to reset the local-state
  [validated-input' value on-change! parse-string to-string valid? props])

(defn input* [value on-change! & [props]]
  ^{:key value}
  [validated-input' value on-change! identity identity (constantly true) props])

(defn keyword-input* [value on-change! & [props]]
  (let [valid? #(let [value (read-string-or-nil %)]
                  (or (nil? value) (keyword? value)))]
    ^{:key value}
    [validated-input' value on-change! read-string-or-nil #(some-> % pr-str) valid? props]))

(defn edn-input* [value on-change! & [props]]
  (let [valid? #(either/branch (safe-read-edn-string %)     ; differentiate between read `nil` and error
                               (constantly false)
                               (constantly true))]
    ^{:key value}
    [validated-input' value on-change! read-string-or-nil #(some-> % pr-str) valid? props]))

(defn id-input [value on-change! & [props]]
  ^{:key (:db/id value)}
  [validated-input' (:db/id value) on-change! read-string-or-nil pr-str read-string-or-nil props])
