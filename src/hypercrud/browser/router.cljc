(ns hypercrud.browser.router                                ; legacy
  (:require
    [contrib.ednish :as ednish]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar]]
    [hyperfiddle.route :as route]))


(defn ^:deprecated encode [route]
  (route/url-encode route [:placeholder-for-legacy/hypercrud.browser.router]))

(defn ^:deprecated decode [s]
  (route/url-decode s [:placeholder-for-legacy/hypercrud.browser.router]))

(defn ^:deprecated -encode-pchar [& args] (apply ednish/encode-uri args))
(defn ^:deprecated -decode-url-ednish [& args] (apply ednish/decode-uri args))
