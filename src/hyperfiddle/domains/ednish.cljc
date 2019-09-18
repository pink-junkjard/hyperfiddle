(ns hyperfiddle.domains.ednish
  (:require
    [clojure.spec.alpha :as s]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.domains.transit :as domains-transit]
    #?(:clj [hyperfiddle.io.datomic :as d])
    [hyperfiddle.io.routes :as routes]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]))


(s/def ::home-route (s/spec :hyperfiddle/route))

(def spec
  (s/and
    (s/keys :req-un [::domain/basis
                     ::domain/fiddle-dbname
                     ::domain/databases
                     ::domain/environment
                     ::home-route])
    #(contains? (:databases %) (:fiddle-dbname %))))

(defrecord EdnishDomain [basis fiddle-dbname databases environment home-route ?datomic-client memoize-cache]
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
  #?(:clj (connect [domain dbname] (d/dyna-connect (domain/database domain dbname) ?datomic-client)))
  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret)))
  )

(domains-transit/register-handlers
  EdnishDomain
  (str 'hyperfiddle.domains.ednish/EdnishDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->EdnishDomain))
