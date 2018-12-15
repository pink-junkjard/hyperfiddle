(ns hyperfiddle.io.hydrate-route
  (:require
    [bidi.bidi :as bidi]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [hyperfiddle.io.http :refer [build-routes]]
    [hyperfiddle.io.http.core :refer [http-request!]]
    [hyperfiddle.io.rpc-router :refer [encode-basis]]
    [promesa.core :as p]))


(defn hydrate-route-rpc! [service-uri build local-basis route branch branch-aux stage & [jwt]]
  ; matrix params instead of path params
  (let [stage (->> stage
                   (remove (comp empty? second))
                   (into {}))]
    (-> (merge {:url (str service-uri (bidi/path-for (build-routes build) :hydrate-route
                                                     :local-basis (encode-basis local-basis)
                                                     :encoded-route (base-64-url-safe/encode (pr-str route))
                                                     ; todo this needs work
                                                     #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                                     :branch (ednish/encode-uri branch)
                                                     :branch-aux (ednish/encode-uri branch-aux)))
                :accept :application/transit+json :as :auto}
               (when jwt {:auth {:bearer jwt}})
               (if (empty? stage)
                 {:method :get}                             ; Try to hit CDN
                 {:method :post
                  :form stage
                  :content-type :application/transit+json}))
        (http-request!)
        (p/then :body))))
