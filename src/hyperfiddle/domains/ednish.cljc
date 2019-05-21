(ns hyperfiddle.domains.ednish
  (:require
    [clojure.spec.alpha :as s]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]))


(s/def ::home-route (s/spec :hyperfiddle/route))

(def spec (s/keys :req-un [::domain/basis
                           ::domain/fiddle-dbname
                           ::domain/databases
                           ::domain/environment
                           ::home-route]))

(defrecord EdnishDomain [basis fiddle-dbname databases environment home-route]
  domain/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "EdnishDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] routes/routes)
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  )
