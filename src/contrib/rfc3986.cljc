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
             [c & ss] s]
        (if-not c
          decoded                                           ; done
          (if (= 37 (char-code c))                          ; 37 is \% written portably
            (recur (conj decoded (hex-str->char (str/join (take 2 ss)))) (drop 2 ss))
            (recur (conj decoded c) ss))))
      str/join))



(defn split-fragment [s]
  (split-first s "#"))

(defn parse-fragment [s]                                 ; user gets full control of this value?
  (second (split-fragment s)))
