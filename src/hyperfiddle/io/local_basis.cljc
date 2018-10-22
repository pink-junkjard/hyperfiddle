(ns hyperfiddle.io.local-basis
  (:require
    [bidi.bidi :as bidi]
    [hypercrud.browser.router :as router]
    [hyperfiddle.io.http :refer [build-routes]]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.rpc-router :refer [encode-basis]]
    [promesa.core :as p]))


(defn local-basis-rpc! [service-uri build global-basis route branch branch-aux & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (build-routes build) :local-basis
                                            :global-basis (encode-basis global-basis)
                                            :encoded-route (subs (router/encode route) 1) ; includes "/"
                                            :branch (router/-encode-pchar branch)
                                            :branch-aux (router/-encode-pchar branch-aux)))
       :accept :application/transit+json :as :auto
       :method :get}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then :body)))
