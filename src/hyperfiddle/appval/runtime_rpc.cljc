(ns hyperfiddle.appval.runtime-rpc
  (:require [cuerdas.core :as str]
            [hypercrud.http.core :refer [http-request!]]
            [hypercrud.util.base-64-url-safe :as base-64-url-safe]
            [promesa.core :as p]
            [taoensso.timbre :as timbre]))


(defn global-basis! [service-uri]
  (-> (http-request! {:url (str #?(:clj service-uri :cljs (.-uri-str service-uri)) "global-basis")
                 :accept :application/transit+json :as :auto
                 :method :get})
      (p/then :body)))

(defn local-basis! [service-uri global-basis encoded-route foo branch]
  (-> {:url (str/format "%(service-uri)slocal-basis/$global-basis/$double-encoded-route/$foo/$branch"
                        {:service-uri (str #?(:clj  service-uri
                                              :cljs (.-uri-str service-uri)))
                         :global-basis (base-64-url-safe/encode (pr-str global-basis))
                         :double-encoded-route (base-64-url-safe/encode encoded-route) ; todo this is awful
                         :foo (base-64-url-safe/encode (pr-str foo))
                         :branch branch})
       :accept :application/transit+json :as :auto
       :method :get}
      (http-request!)
      (p/then :body)))

(defn hydrate-route! [service-uri local-basis encoded-route foo branch stage]
  (-> (merge {:url (str/format "%(service-uri)shydrate-route/$local-basis/$double-encoded-route/$foo/$branch"
                               {:service-uri (str #?(:clj  service-uri
                                                     :cljs (.-uri-str service-uri)))
                                :local-basis (base-64-url-safe/encode (pr-str local-basis))
                                :double-encoded-route (base-64-url-safe/encode encoded-route) ; todo this is awful
                                :foo (base-64-url-safe/encode (pr-str foo))
                                :branch (base-64-url-safe/encode (pr-str branch))})
              :accept :application/transit+json :as :auto}
             (if (empty? stage)
               {:method :get}                               ; Try to hit CDN
               {:method :post
                :form stage
                :content-type :application/transit+json}))
      (http-request!)
      (p/then :body)))
