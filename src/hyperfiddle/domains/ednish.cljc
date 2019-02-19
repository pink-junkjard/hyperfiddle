(ns hyperfiddle.domains.ednish
  (:require
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]))


(defrecord EdnishDomain [ident fiddle-dbname databases environment home-route service-uri build]
  domain/Domain
  (ident [domain] ident)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] (routes/build build))
  (service-uri [domain] service-uri)
  )
