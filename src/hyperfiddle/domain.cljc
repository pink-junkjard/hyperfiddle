(ns hyperfiddle.domain)


(defprotocol Domain
  (ident [domain])
  (fiddle-database [domain])
  (databases [domain])
  (environment [domain])
  (url-decode [domain s])
  (url-encode [domain route])
  )

(defn database [domain dbname]
  (if (= dbname 'hyperfiddle.domain/fiddle-database)
    (fiddle-database domain)
    (get (databases domain) dbname)))
