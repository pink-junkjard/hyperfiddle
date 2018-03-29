(ns hyperfiddle.io.local-basis
  (:require [cuerdas.core :as str]
            [hyperfiddle.io.http.core :refer [http-request!]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [promesa.core :as p]))


(defn local-basis-rpc! [service-uri global-basis route branch branch-aux]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$encoded-route/$branch/$branch-aux"
                        {:service-uri service-uri
                         :global-basis (base-64-url-safe/encode (pr-str global-basis))
                         :encoded-route (base-64-url-safe/encode (pr-str route))
                         :branch (base-64-url-safe/encode (pr-str branch))
                         :branch-aux (base-64-url-safe/encode (pr-str branch-aux))})
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request!)
      (p/then :body)))
