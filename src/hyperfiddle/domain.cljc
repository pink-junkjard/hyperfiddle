(ns hyperfiddle.domain
  (:refer-clojure :exclude [memoize])
  (:require
    [hyperfiddle.service.resolve :as R]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]
    [hyperfiddle.database.color :as color]
    [cognitect.transit :as t]
    [hypercrud.transit :as hc-t]
    #?(:clj [hyperfiddle.io.datomic.core :as d])
    [hypercrud.browser.router-bidi :as router-bidi]

    [bidi.bidi :as bidi]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [contrib.uri :refer [is-uri?]]
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]))


; todo these db specs belong somewhere else
(def dbname-spec (s/and string? #(string/starts-with? % "$")))
(s/def :database/db-name string?)
(s/def :database/uri is-uri?)
(def database-spec (s/keys :opt [:database/db-name
                                 :database/uri
                                 :database/write-security
                                 :hf/transaction-operation-whitelist]))
(s/def :database/write-security (s/keys :req [:db/ident]))  ;  {:db/ident :hyperfiddle.security/tx-operation-whitelist}
(s/def :hf/transaction-operation-whitelist (s/coll-of keyword?)) ; [:school/street-address :sub/phone-confirmed :sub-req/subject]

(s/def ::config map?)                                       ; can validate config too
(s/def ::basis some?)
(s/def ::type-name some?)
(s/def ::fiddle-dbname dbname-spec)
(s/def ::databases (s/map-of dbname-spec database-spec))
(s/def ::environment map?)


(defprotocol Domain
  (basis [domain])
  (type-name [domain])
  (fiddle-dbname [domain])
  (databases [domain])
  (environment [domain])
  (url-decode [domain s])
  (url-encode [domain route])
  (api-routes [domain])

  (system-fiddle? [domain fiddle-ident])
  (hydrate-system-fiddle [domain fiddle-ident])
  #?(:clj (connect [domain dbname] [domain dbname on-created!]))
  (memoize [domain f]))


(defn database [domain dbname]
  (get (databases domain) dbname))

(defn database-color [domain dbname]
  (or (:database/color (database domain dbname)) (color/color-for-name dbname)))

(defn api-path-for [domain handler & {:as params}]
  (apply bidi/path-for (api-routes domain) handler (apply concat params)))

(defn api-match-path [domain path & {:as options}]
  (apply bidi/match-route (api-routes domain) path (apply concat options)))

(defn valid-dbname? [domain dbname] (some? (database domain dbname)))

(defn valid-dbnames? [domain dbnames] (set/subset? (set dbnames) (set (keys (databases domain)))))

(defn dbname-label [dbname]
  (if (= "$" dbname)
    "$ (default)"
    dbname))

(s/def ::home-route (s/spec :hyperfiddle/route))

(def spec-ednish-domain
  (s/and
    (s/keys :req-un [:hyperfiddle.config/config
                     ::basis
                     ::fiddle-dbname
                     ::databases
                     ::environment
                     ::home-route])
    #(contains? (:databases %) (:fiddle-dbname %))))

(defrecord EdnishDomain [config basis fiddle-dbname databases environment home-route ?datomic-client memoize-cache]
  Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "EdnishDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] R/domain-routes)
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  #?(:clj (connect [domain dbname] (d/dyna-connect (database domain dbname) ?datomic-client)))
  #?(:clj (connect [domain dbname on-created!] (d/dyna-connect (database domain dbname) ?datomic-client on-created!)))
  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

(defrecord BidiDomain [config basis fiddle-dbname databases environment router ?datomic-client memoize-cache]
  Domain
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
  (api-routes [domain] (R/domain-routes config))
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  #?(:clj (connect [domain dbname] (d/dyna-connect (database domain dbname) ?datomic-client)))
  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

(hypercrud.transit/register-handlers
  EdnishDomain
  (str 'hyperfiddle.domain/EdnishDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->EdnishDomain))

(hypercrud.transit/register-handlers
  BidiDomain
  (str 'hyperfiddle.domain/BidiDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->BidiDomain))
