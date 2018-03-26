(ns hypercrud.browser.router
  (:require [cuerdas.core :as str]
            [hypercrud.util.core :refer [split-first]]
            [hypercrud.compile.reader :as reader]
            [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar encode-ednish decode-ednish]]))


(def -encode-pchar (comp encode-rfc3986-pchar encode-ednish pr-str)) ; strings get quotes, its okay
(def -decode-url-ednish (comp reader/read-string decode-ednish decode-rfc3986-pchar))

(comment
  {:fiddle-id :hyperfiddle.blog/post
   :request-params [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]]})

(defn -encode-fiddle-id [fiddle]
  (if (vector? fiddle)
    (let [[a v] fiddle]
      (if (= a :fiddle/ident)
        v
        fiddle))
    fiddle))

(defn encode [route]
  (let [fiddle (:fiddle-id route)                           ; ident, long (not entity - $ is extraneous)
        fiddle-args (->> (dissoc route :fiddle-id :request-params) sort vals) ; positional
        datomic-args (:request-params route)
        service-args {}
        state {}]
    (str "/"
         (str/join ";" (->> (cons (-encode-pchar fiddle) (map -encode-pchar fiddle-args))))
         "/"
         (str/join "/" (map -encode-pchar datomic-args))    ; datomic args as path params is sensible default for userland

         ; hash and query aren't used today, todo i would prefer to encode as edn hashmap instead of k=v
         (if (seq service-args) (str "?" (str/join "&" (->> service-args (map (fn [[k v]] (-encode-pchar k "=" (-encode-pchar v))))))))
         (if (seq state) (str "#" (str/join "&" (->> state (map (fn [[k v]] (-encode-pchar k "=" (-encode-pchar v)))))))))))

(defn decode [s]
  (let [[root s] (split-first s "/")
        [fiddle-segment s] (split-first s "/")
        [fiddle & fiddle-args] (str/split fiddle-segment ";")
        [datomic-args-segment s] (split-first s "?")
        datomic-args (->> (str/split datomic-args-segment "/")) ; careful: (str/split "" "/") => [""]
        [query fragment] (split-first s "#")]
    (merge
      {:fiddle-id (-decode-url-ednish fiddle)}
      (if-let [as (seq (remove str/empty-or-nil? datomic-args))]
        {:request-params (mapv -decode-url-ednish as)})
      #_(map -decode-url-ednish fiddle-args)                ; fiddle-args is not a map, its positional
      #_{:state (-decode-url-ednish fragment)
         :service-args (-decode-url-ednish query)})))
