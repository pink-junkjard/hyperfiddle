(ns hypercrud.browser.link
  (:require
    [cats.core :refer [>>=]]
    [contrib.ct :refer [unwrap]]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.try$ :refer [try-either]]
    [taoensso.timbre :as timbre]))


(defn popover-link? [link] (boolean (:link/managed? link)))

(defn read-path [s]
  (->> (memoized-safe-read-edn-string (str "[" s "]"))
       (unwrap #(timbre/error %))))                         ; too late to report anything to the dev

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))

(defn links-at [path links-ref]
  (filter (partial same-path-as? path) @links-ref))
