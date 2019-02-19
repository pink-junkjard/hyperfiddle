(ns hyperfiddle.domains.bidi
  (:require
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]))


(defrecord BidiDomain [basis ident fiddle-dbname databases environment router service-uri build]
  domain/Domain
  (basis [domain] basis)
  (ident [domain] ident)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s]
    (either/branch
      (try-either (router-bidi/decode router s))
      (fn [e] (route/decoding-error e s))
      identity))
  (url-encode [domain route] (router-bidi/encode router route))
  (api-routes [domain] (routes/build build))
  (service-uri [domain] service-uri)
  )
