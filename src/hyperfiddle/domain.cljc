(ns hyperfiddle.domain)


(defn uri->hfdb [uri domain]
  {:post [%]}
  (if (= uri (get-in domain [:domain/fiddle-database :database/uri]))
    (:domain/fiddle-database domain)
    (->> (:domain/databases domain)
         (map :domain.database/record)
         (some #(when (= uri (:database/uri %)) %)))))

(defn dbname->hfdb [dbname domain]
  {:post [%]}
  (->> (:domain/databases domain)
       (some #(when (= dbname (:domain.database/name %)) %))
       :domain.database/record))

(defn uri->dbname [uri domain]
  (->> (:domain/databases domain)
       (some #(when (= uri (get-in % [:domain.database/record :database/uri])) %))
       :domain.database/name))

(defn dbname->uri [dbname domain]
  (:database/uri (dbname->hfdb dbname domain)))
