(ns hypercrud.browser.link
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.reader :refer [memoized-read-edn-string+]]
    [taoensso.timbre :as timbre]))


(defn read-path [s]
  (->> (memoized-read-edn-string+ (str "[" s "]"))
       (unwrap #(timbre/error %))))                         ; too late to report anything to the dev

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))
