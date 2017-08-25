(ns hypercrud.browser.routing
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.util.base-64-url-safe :as base64]))

(defn slugify [s] s)

(defn encode [route]
  (if-not route
    "/"
    (do
      ;(assert (:project route) "missing project name") - be safe - maybe making the link now
      (str "/" (slugify (or (:project route) "foo")) "/" (base64/encode (pr-str route))))))

(defn decode [route-str]
  (assert (string/starts-with? route-str "/"))

  ; Urls in the wild get query params added because tweetdeck tools think its safe e.g.:
  ; http://localhost/hyperfiddle-blog/ezpkb21haW4gbmlsLCA6cHJvamVjdCAiaHlwZXJmaWRkbGUtYmxvZyIsIDpsaW5rLWRiaWQgI0RiSWRbMTc1OTIxODYwNDU4OTQgMTc1OTIxODYwNDU0MjJdLCA6cXVlcnktcGFyYW1zIHs6ZW50aXR5ICNEYklkWzE3NTkyMTg2MDQ2MTMyIDE3NTkyMTg2MDQ1ODgyXX19?utm_content=buffer9a24a&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer


  (let [[_ project-name route-encoded-and-query-params] (string/split route-str #"/")]
    (cond
      (not (nil? route-encoded-and-query-params))
      (let [[route-encoded url-param-garbage] (string/split route-encoded-and-query-params #"\?")]
        (reader/read-string (base64/decode route-encoded)))

      ;deduce the project's index-link
      ;(not (nil? project-name)) {:project (unslugify project-name)}

      ; The only way to get to / is if the user types it in. We never ever link to /, and nginx & node should redirect to the canonical.
      :else nil)))
