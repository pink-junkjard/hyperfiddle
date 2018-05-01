(ns hypercrud.browser.router
  (:require [contrib.data :refer [rtrim-coll orp]]
            [contrib.reader :as reader]
            [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar encode-ednish decode-ednish]]
            [contrib.string :refer [split-first empty->nil]]
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

(defn encode [[fiddle datomic-args service-args frag :as route]]
  {:post [(str/starts-with? % "/")]}
  ; fiddle is keyword (not entity - $ is extraneous)
  (let [fiddle-args []]
    (str "/"
         (str/join ";" (->> (cons (-encode-pchar fiddle) (map -encode-pchar fiddle-args))))
         "/"
         (str/join "/" (map -encode-pchar datomic-args))    ; datomic args as path params is sensible default for userland

         ; hash and query aren't used today, todo i would prefer to encode as edn hashmap instead of k=v
         (if (seq service-args) (str "?" (str/join "&" (->> service-args (map (fn [[k v]] (-encode-pchar k "=" (-encode-pchar v))))))))
         (if (empty->nil frag) (str "#" (-> frag encode-ednish encode-rfc3986-pchar))))))

(defn canonicalize "(apply canonicalize route)"
  [& [fiddle #_?fiddle-args ?datomic-args ?service-args ?initial-state]]
  (orp empty? (rtrim-coll nil? [fiddle ?datomic-args ?service-args ?initial-state])))

(defn decode [s]
  (let [[root s] (split-first s "/")
        [fiddle-segment s] (split-first s "/")
        [fiddle & fiddle-args] (str/split fiddle-segment ";")
        [s frag] (split-first s "#")
        [datomic-args-segment query] (split-first s "?")
        datomic-args (->> (str/split datomic-args-segment "/"))] #_"careful: (str/split \"\" \"/\") => [\"\"]"

    (canonicalize
      (-decode-url-ednish fiddle)
      ;(mapv -decode-url-ednish fiddle-args)
      (if-let [as (->> datomic-args (remove str/empty-or-nil?) seq)]
        (mapv -decode-url-ednish as))
      (-decode-url-ednish query)
      (-> frag decode-rfc3986-pchar decode-ednish empty->nil))))

(defn assoc-frag [[fiddle ?datomic-args ?service-args ?initial-state] frag]
  {:pre [fiddle (nil? ?initial-state)]}
  (let [x (canonicalize fiddle ?datomic-args ?service-args frag)]
    x))

(defn dissoc-frag [[fiddle ?datomic-args ?service-args _]]
  ;{:pre [fiddle]} ; Dissoc-frag should nil-pun for bootstrapping case
  (let [x (canonicalize fiddle ?datomic-args ?service-args nil)]
    x))
