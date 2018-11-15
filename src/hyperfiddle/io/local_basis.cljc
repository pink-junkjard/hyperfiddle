(ns hyperfiddle.io.local-basis
  (:require
    [bidi.bidi :as bidi]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [hyperfiddle.io.http :refer [build-routes]]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.rpc-router :refer [encode-basis]]
    [promesa.core :as p]))


(defn local-basis-rpc! [service-uri build global-basis route branch branch-aux & [jwt]]
  (-> {:url (str service-uri (bidi/path-for (build-routes build) :local-basis
                                            :global-basis (encode-basis global-basis)
                                            :encoded-route (base-64-url-safe/encode (pr-str route))
                                            ; todo this needs work
                                            #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                            :branch (ednish/encode-uri branch)
                                            :branch-aux (ednish/encode-uri branch-aux)))
       :accept :application/transit+json :as :auto
       :method :get}
      (into (when jwt {:auth {:bearer jwt}}))
      (http-request!)
      (p/then :body)))
