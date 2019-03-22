(ns hyperfiddle.domains.bidi
  (:require
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]))


(defrecord BidiDomain [basis fiddle-dbname databases environment router build]
  domain/Domain
  (basis [domain] basis)
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
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  )
