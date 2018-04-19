(ns hypercrud.browser.router
  (:require [contrib.data :refer [rtrim-coll]]
            [contrib.reader :as reader]
            [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar encode-ednish decode-ednish]]
            [contrib.string :refer [split-first]]
            [cuerdas.core :as str]))


(def -encode-pchar (comp encode-rfc3986-pchar encode-ednish pr-str)) ; strings get quotes, its okay
(def -decode-url-ednish (comp reader/read-string decode-ednish decode-rfc3986-pchar))

(comment
  [:hyperfiddle.blog/post [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]]])

(defn -encode-fiddle-id [fiddle]
  (if (vector? fiddle)
    (let [[a v] fiddle]
      (if (= a :fiddle/ident)
        v
        fiddle))
    fiddle))

(defn encode [route]
  ; fiddle is keyword (not entity - $ is extraneous)
  (let [[fiddle datomic-args] route
        fiddle-args []
        service-args {}
        state {}]
    (str "/"
         (str/join ";" (->> (cons (-encode-pchar fiddle) (map -encode-pchar fiddle-args))))
         "/"
         (str/join "/" (map -encode-pchar datomic-args))    ; datomic args as path params is sensible default for userland

         ; hash and query aren't used today, todo i would prefer to encode as edn hashmap instead of k=v
         (if (seq service-args) (str "?" (str/join "&" (->> service-args (map (fn [[k v]] (-encode-pchar k "=" (-encode-pchar v))))))))
         (if (seq state) (str "#" (str/join "&" (->> state (map (fn [[k v]] (-encode-pchar k "=" (-encode-pchar v)))))))))))

(defn canonicalize "(apply canonicalize route)"
  [& [fiddle #_?fiddle-args ?datomic-args ?service-args ?initial-state]]
  (rtrim-coll nil? [fiddle ?datomic-args ?service-args ?initial-state]))

(defn decode [s]
  (let [[root s] (split-first s "/")
        [fiddle-segment s] (split-first s "/")
        [fiddle & fiddle-args] (str/split fiddle-segment ";")
        [s fragment] (split-first s "#")
        [datomic-args-segment query] (split-first s "?")
        datomic-args (->> (str/split datomic-args-segment "/"))] #_ "careful: (str/split \"\" \"/\") => [\"\"]"

    (canonicalize
      (-decode-url-ednish fiddle)
      ;(mapv -decode-url-ednish fiddle-args)
      (if-let [as (->> datomic-args (remove str/empty-or-nil?) seq)]
        (mapv -decode-url-ednish as))
      (-decode-url-ednish query)
      fragment)))

(defn assoc-frag [[fiddle ?datomic-args ?service-args ?initial-state] frag]
  {:pre [(nil? ?initial-state)]}
  (let [x (canonicalize fiddle ?datomic-args ?service-args frag)]
    x))
