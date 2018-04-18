(ns contrib.rfc3986
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [contrib.char :refer [char-code dec->hex hex->dec char->hex-str hex-str->char]]
            [contrib.string :refer [split-first]]))


; https://tools.ietf.org/html/rfc3986#appendix-A
;
; pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
; unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
; sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="

(def -sub-delims #{\! \$ \& \' \( \) \* \+ \, \; \=})

(def -unreserved (set/union (set (map char "abcdefghijklmnopqrstuvwxyz"))
                            (set (map char "0123456789"))
                            #{\- \. \_ \~}))

(def -pchar (set/union -unreserved -sub-delims #{\: \@}))

(defn encode-rfc3986-pchar "precent-encode a url path segment without over-encoding. The usual platform url decoders
over-encode by a lot. Notably, this is compatible with java.net.URI which fails on some chars in the 'unwise set', which
are probably safe today."
  [s]
  (->> s
       (map (fn [c]
              (if (-pchar c)                                ; whitelist
                c
                (str "%" (char->hex-str c)))))
       (str/join)))

(defn decode-rfc3986-pchar [s]
  (-> (loop [decoded []
             [c & ss] (seq s)]
        (if-not c
          decoded                                           ; done
          (if (= 37 (char-code c))                          ; 37 is \% written portably
            (recur (conj decoded (hex-str->char (str/join (take 2 ss)))) (drop 2 ss))
            (recur (conj decoded c) ss))))
      str/join))

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
  (reduce (fn [a [k v]] (str/replace a k v)) edn-str -edn-dialect-mappings))

(defn decode-ednish [ednish-str]
  (reduce (fn [a [k v]] (str/replace a k v)) ednish-str (set/map-invert -edn-dialect-mappings)))

(defn split-fragment [s]
  (split-first s "#"))

(defn parse-fragment [s]                                 ; user gets full control of this value?
  (second (split-fragment s)))
