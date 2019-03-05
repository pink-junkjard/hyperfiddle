(ns contrib.ednish
  (:require
    [clojure.set]
    [clojure.string :as string]
    [contrib.reader :as reader]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar]]))


; https://tools.ietf.org/html/rfc2396#section-2.4.3
;
; unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"

(def -edn-dialect-mappings
  {\space \,
   \" \'
   \# \~
   \/ \!
   ;\{ \( \} \)
   \[ \( \] \)
   })

; Paste this into chrome and it will display properly
; https://ko.wikipedia.org/wiki/%EC%9C%84%ED%82%A4%EB%B0%B1%EA%B3%BC:%EB%8C%80%EB%AC%B8

(defn encode-ednish "Re-encode an edn-string to url-safe dialect of edn-ish. Vectors, sets and maps
coalesce into lists and are not disambiguated."
  [edn-str]
  (reduce (fn [a [k v]]
            (string/replace a k v))
          edn-str
          -edn-dialect-mappings))

(defn decode-ednish [ednish-str]
  (reduce (fn [a [k v]] (string/replace a k v))
          ednish-str
          (clojure.set/map-invert -edn-dialect-mappings)))

(def encode-uri (comp encode-rfc3986-pchar encode-ednish pr-str))
(def decode-uri (comp reader/read-edn-string! decode-ednish decode-rfc3986-pchar))
