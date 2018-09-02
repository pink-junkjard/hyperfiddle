(ns hypercrud.browser.link
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [taoensso.timbre :as timbre]))


(defn read-path [s]
  (->> (memoized-safe-read-edn-string (str "[" s "]"))
       (unwrap #(timbre/error %))))                         ; too late to report anything to the dev

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))

(defn links-at [path links-ref]
  (filter (partial same-path-as? path) @links-ref))
