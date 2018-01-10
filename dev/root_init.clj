(ns root-init
  (:require [clojure.java.io :as io]
            [datomic.api :as d]))


(defn generate-domain-record [transactor-uri db-name]
  {:db/id (d/tempid :db.part/user)
   ; todo https://tools.ietf.org/html/rfc3986#section-2
   :domain/ident db-name})

(defn init-root [root-uri hf-schema hf-data]
  (if-not (d/create-database root-uri)
    (throw (new Error "Cannot initialize from existing database"))

    (let [root-conn (d/connect root-uri)]
      (println "Installing root schema")
      @(d/transact root-conn hf-schema)

      (when hf-data
        (println "Installing hyperfiddle data")
        @(d/transact root-conn hf-data)))))

(defn reflect-user-dbs [root-uri transactor-uri]
  (println "Reflecting existing user databases")
  (doseq [db-name (->> (d/get-database-names (str transactor-uri "*"))
                       (remove #(= root-uri (str transactor-uri %))))]
    (println (str "Registering db: " db-name))
    @(d/transact (d/connect root-uri) [(generate-domain-record transactor-uri db-name)])))

(defn init
  ([root-uri transactor-uri hf-schema]
   (init root-uri transactor-uri hf-schema nil))
  ([root-uri transactor-uri hf-schema hf-data]
   (init-root root-uri hf-schema hf-data)
   (reflect-user-dbs root-uri transactor-uri)
   (println "Finished initializing")))

(defn init-from-file
  ([root-uri transactor-uri hf-schema-src]
   (assert (.exists (io/file hf-schema-src)))
   (init-from-file root-uri transactor-uri hf-schema-src nil))
  ([root-uri transactor-uri hf-schema-src hf-data-src]
   (assert (.exists (io/file hf-schema-src)))
   (init root-uri
         transactor-uri
         (-> hf-schema-src slurp read-string)
         (some-> hf-data-src slurp read-string))))
