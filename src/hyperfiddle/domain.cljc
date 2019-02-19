(ns hyperfiddle.domain
  (:require
    [bidi.bidi :as bidi]
    [hyperfiddle.database.color :as color]))


(defprotocol Domain
  (ident [domain])
  (fiddle-database [domain])
  (databases [domain])
  (environment [domain])
  (url-decode [domain s])
  (url-encode [domain route])
  (api-routes [domain])
  (service-uri [domain])
  )

(defn database [domain dbname]
  (if (= dbname 'hyperfiddle.domain/fiddle-database)
    (fiddle-database domain)
    (get (databases domain) dbname)))

(defn database-color [domain dbname]
  (or (:database/color (database domain dbname)) (color/color-for-name dbname)))

(defn api-url-for [domain handler & {:as params}]
  (str (service-uri domain) (apply bidi/path-for (api-routes domain) handler (apply concat params))))

(defn api-match-path [domain path & {:as options}]
  (apply bidi/match-route (api-routes domain) path (apply concat options)))
