(ns hyperfiddle.io.local-basis
  (:require
    [contrib.base-64-url-safe :as base-64-url-safe]
    [cuerdas.core :as str]
    [hypercrud.browser.router :as router]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [promesa.core :as p]))


(defn local-basis-rpc! [service-uri global-basis route branch branch-aux & [jwt]]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$branch/$branch-aux$encoded-route"
                        {:service-uri service-uri
                         :global-basis (router/-encode-pchar global-basis)
                         :encoded-route (router/encode route) ; includes "/"
                         :branch (router/-encode-pchar branch)
                         :branch-aux (router/-encode-pchar branch-aux)})
       :accept :application/transit+json :as :auto
       :method :get}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then :body)))
