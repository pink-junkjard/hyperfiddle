(ns hyperfiddle.domains.ednish
  (:require
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]))


(defrecord EdnishDomain [basis ident fiddle-dbname databases environment home-route build]
  domain/Domain
  (basis [domain] basis)
  (ident [domain] ident)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] (routes/build build))
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  )
