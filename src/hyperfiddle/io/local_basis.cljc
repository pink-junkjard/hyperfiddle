(ns hyperfiddle.io.local-basis
  (:require [cuerdas.core :as str]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [hyperfiddle.foundation :as foundation]
            [hyperfiddle.io.hydrate-requests :refer [hydrate-one!]]
            [promesa.core :as p]))


(defn fetch-domain! [rt hostname hyperfiddle-hostname global-basis]
  (-> (hydrate-one! rt (apply sorted-map (apply concat (:domain global-basis))) nil
                    (foundation/domain-request
                      (foundation/hostname->hf-domain-name hostname hyperfiddle-hostname) rt))))

(defn local-basis-rpc! [service-uri global-basis encoded-route foo branch]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$double-encoded-route/$foo/$branch"
                        {:service-uri service-uri
                         :global-basis (base-64-url-safe/encode (pr-str global-basis))
                         :double-encoded-route (base-64-url-safe/encode encoded-route) ; todo this is awful
                         :foo (base-64-url-safe/encode (pr-str foo))
                         :branch branch})
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request!)
      (p/then :body)))
