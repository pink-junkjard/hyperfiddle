(ns contrib.document
  (:require [contrib.string :refer [empty->nil]]
            [cuerdas.core :as str]))


(defn hostname! [] js/document.location.hostname)

(defn fragment! []
  (empty->nil (str/strip-prefix js/document.location.hash "#")))

(defn path! []
  js/document.location.pathname)

(defn root-rel-url! []
  (str (path!)
       (if-let [frag (fragment!)]
         (str "#" frag))))
