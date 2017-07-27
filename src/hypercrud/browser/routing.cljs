(ns hypercrud.browser.routing
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [hypercrud.client.core :as hc]
            [hypercrud.types.DbId :refer [->DbId]]
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

  (let [[_ project-name encoded-params] (string/split route-str #"/")]
    (cond
      (not (nil? encoded-params)) (reader/read-string (base64/decode encoded-params))

      ;deduce the project's index-link
      ;(not (nil? project-name)) {:project (unslugify project-name)}

      ; The only way to get to / is if the user types it in. We never ever link to /, and nginx & node should redirect to the canonical.
      :else
      {:domain nil
       :project "hyperfiddle-blog"
       :link-dbid (->DbId [:link/ident :hyperfiddle.blog/post] hc/*root-conn-id*)
       :query-params {:entity (->DbId [:hypercrud/ident :hyperfiddle.blog/homepage]
                                      [:database/ident "hyperfiddle-blog"])}})))
