(ns hyperfiddle.domains.bidi
  (:require
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.router-bidi :as router-bidi]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.transit :as domains-transit]
    #?(:clj [hyperfiddle.io.datomic :as d])
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]))


(defrecord BidiDomain [basis fiddle-dbname databases environment router ?datomic-client]
  domain/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "BidiDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s]
    (either/branch
      (try-either (router-bidi/decode router s))
      (fn [e] (route/decoding-error e s))
      identity))
  (url-encode [domain route] (router-bidi/encode router route))
  (api-routes [domain] routes/routes)
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  #?(:clj (connect [domain dbname] (d/dyna-connect (domain/database domain dbname) ?datomic-client))))

(domains-transit/register-handlers
  BidiDomain
  (str 'hyperfiddle.domains.bidi/BidiDomain)
  #(-> (into {} %) (dissoc :?datomic-client))
  map->BidiDomain)
