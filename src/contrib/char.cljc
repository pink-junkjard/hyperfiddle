(ns contrib.char
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(def dec->hex {0 \0 1 \1 2 \2 3 \3 4 \4 5 \5 6 \6 7 \7 8 \8 9 \9 10 \a 11 \b 12 \c 13 \d 14 \e 15 \f})
(def hex->dec (set/map-invert dec->hex))

(defn char-code [c]
  {:pre [c]}
  ; cljs.user=> (map identity "abcd") => ("a" "b" "c" "d")
  ; cljs.user=> (map #(.charCodeAt %) "abcd") => (97 98 99 100)
  ;
  ; (map identity "abcd") => (\a \b \c \d)
  ; (map int "abcd") => (97 98 99 100)
  #?(:cljs (.charCodeAt c)
     :clj  (int c)))

(defn char->hex-str [c]
  {:pre [c]}
  ; Portable - the usual primitives are platform specific and weird interfaces
  ;{:pre [(< n 256) (> n 0)]}
  (->> (char-code c)
       ((juxt #(quot % 16) #(mod % 16)))
       (map dec->hex)
       str/join))

(defn hex-str->char [s]
  {:pre [(= 2 (count s))]}
  (let [[a b] (map hex->dec s)]
    (char (+ (* 16 (int a)) (int b)))))
