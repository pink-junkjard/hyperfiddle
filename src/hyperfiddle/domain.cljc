(ns hyperfiddle.domain)


(defn uri->dbname [uri domain]
  (->> (:domain/databases domain)
       (some #(when (= uri (get-in % [:domain.database/record :database/uri])) %))
       :domain.database/name))

(defn dbname->uri [dbname domain]
  (->> (:domain/databases domain)
       (some #(when (= dbname (:domain.database/name %)) %))
       :domain.database/record
       :database/uri))

(defn db-for-uri [uri domain]
  (if (= uri (get-in domain [:domain/fiddle-database :database/uri]))
    (:domain/fiddle-database domain)
    (->> (:domain/databases domain)
         (some #(when (= uri (get-in % [:domain.database/record :database/uri])) %)))))
