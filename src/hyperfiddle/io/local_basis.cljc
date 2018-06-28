(ns hyperfiddle.io.local-basis
  (:require
    [contrib.rfc3986 :refer [encode-rfc3986-pchar decode-rfc3986-pchar]]
    [cuerdas.core :as str]
    [hypercrud.browser.router :as router]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.rpc-router :refer [encode-basis]]
    [promesa.core :as p]))


(defn local-basis-rpc! [service-uri global-basis route branch branch-aux & [jwt]]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$branch/$branch-aux$encoded-route"
                        {:service-uri service-uri
                         :global-basis (encode-basis global-basis)
                         :encoded-route (router/encode route) ; includes "/"
                         :branch (router/-encode-pchar branch)
                         :branch-aux (router/-encode-pchar branch-aux)})
       :accept :application/transit+json :as :auto
       :method :get}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then :body)))
