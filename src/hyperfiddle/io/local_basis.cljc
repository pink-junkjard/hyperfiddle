(ns hyperfiddle.io.local-basis
  (:require [cuerdas.core :as str]
            [hypercrud.browser.routing :as routing]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [promesa.core :as p]))


; unused ??
(defn local-basis-rpc! [service-uri global-basis route foo branch]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$encoded-route/$foo/$branch"
                        {:service-uri service-uri
                         :global-basis (base-64-url-safe/encode (pr-str global-basis))
                         :encoded-route (subs (routing/encode route) 1) #_"drop leading /"
                         :foo (base-64-url-safe/encode (pr-str foo))
                         :branch branch})
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request!)
      (p/then :body)))
