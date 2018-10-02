(ns contrib.document
  (:require
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.string :refer [empty->nil]]
    [cuerdas.core :as str]))


(code-for-browser
  (defn fragment! []
    (empty->nil (str/strip-prefix js/document.location.hash "#"))))

(code-for-browser
  (defn root-rel-url! []
    (str js/document.location.pathname
         (if-let [frag (fragment!)]
           (str "#" frag)))))
