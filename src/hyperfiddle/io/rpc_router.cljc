(ns hyperfiddle.io.rpc-router
  (:require
    [clojure.string]
    [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar]]
    [contrib.reader :as reader]
    [contrib.uri :refer [->URI]]))


(defn encode-basis [basis]
  ; The ednish encoder is not smart enough to leave ~ intact in string literals
  (->> basis
       (map (juxt (comp str first) second))
       (mapcat identity)
       (map pr-str)
       (interpose ",")
       (apply str)
       encode-rfc3986-pchar))

(defn decode-basis [basis-str]
  (-> basis-str
      decode-rfc3986-pchar
      (clojure.string/split #",")
      (->> (map reader/read-string))
      (->> (partition 2))
      (->> (map (juxt (comp ->URI first) second)))))
