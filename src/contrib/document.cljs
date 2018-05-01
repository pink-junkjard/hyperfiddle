(ns contrib.document
  (:require
    [contrib.cljs-platform :as cljs-platform]
    [contrib.string :refer [empty->nil]]
            [cuerdas.core :as str]))


(defn hostname! []
  {:pre [(cljs-platform/browser?)]}
  js/document.location.hostname)

(defn fragment! []
  {:pre [(cljs-platform/browser?)]}
  (empty->nil (str/strip-prefix js/document.location.hash "#")))

(defn path! []
  {:pre [(cljs-platform/browser?)]
   :post [(str/starts-with? % "/")]}
  js/document.location.pathname)

(defn root-rel-url! []
  {:pre [(cljs-platform/browser?)]}
  (str (path!)
       (if-let [frag (fragment!)]
         (str "#" frag))))
