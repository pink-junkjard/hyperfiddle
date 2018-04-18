(ns hypercrud.browser.router-base64
  (:require [cuerdas.core :as str]
            [contrib.base-64-url-safe :as base64]
            [contrib.reader :as reader]))


; Keeping this around because the ednish router has issues
; Could be interesting as a userland opt-in too


(defn encode [route]
  ;/-/code-database    :fiddle-ident         ;fiddle-params       /query-params
  ;"/-/starter-blog-src2:schema!all-attributes;dbhole!name=$domains/"
  (str "/" (some-> route pr-str base64/encode)))

(defn decode [route-str]
  ; route-str must be well-formed (canonical redirect already happened)
  ; but is potentially invalid garbage from bots
  {:pre [(seq route-str)
         (str/starts-with? route-str "/")
         #_(not= "/" route-str)]
   :post [#_(do (println % route-str) true)
          #_(if % (:fiddle-id %))]}
  ; Urls in the wild get query params added because tweetdeck tools think its safe e.g.:
  ; http://localhost/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MTMyIDE3NTkyMTg2MDQ1ODgyXX19?utm_content=buffer9a24a&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  (let [[_ route-encoded-and-query-params] (str/split route-str #"/")]
    (cond
      (not (nil? route-encoded-and-query-params))
      (let [[route-encoded url-param-garbage] (str/split route-encoded-and-query-params #"\?")]
        (reader/read-string (base64/decode route-encoded)))

      ; no route, or garbage from http crawlers
      :else nil)))
