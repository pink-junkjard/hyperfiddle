(ns hypercrud.server.datomic.root-init
  (:require [clojure.java.io :as io]
            [datomic.api :as d]))


(defn generate-db-record [db-name]
  {:db/id (d/tempid :db.part/user)
   ; todo https://tools.ietf.org/html/rfc3986#section-2
   :database/ident db-name})

(defn reflect-schema [conn]
  (let [$ (d/db conn)]
    (->> (d/q '[:find [?a ...] :in $ :where [?a :db/ident]] $)
         (mapv #(d/touch (d/entity $ %)))
         ;filter out datomic attributes, todo this is a huge hack
         (filter #(> (:db/id %) 62)))))

; root isn't parameterized that well
; the current hf-src will create a database for root AND hyperfiddle AND hyperfiddle-users
; this means 3 databases on top of existing infra :(
(defn init
  ([transactor-uri hf-schema]
   (let [hf-data
         ; the only reason this root db-record exists in this loader is to satisfy init-datomic
         ; it can be deleted when hypercrud.server.db-root/root-id is
         [(generate-db-record "root")]
         ]
     (init transactor-uri hf-schema hf-data)))
  ([transactor-uri hf-schema hf-data]
   (let [root-transactor-uri (str transactor-uri "root")]
     (if-not (d/create-database root-transactor-uri)
       (throw (new Error "Cannot initialize from existing database"))

       (let [root-conn (d/connect root-transactor-uri)]
         (println "Installing root schema")
         @(d/transact root-conn hf-schema)

         (when hf-data
           (println "Installing hyperfiddle data")
           @(d/transact root-conn hf-data))

         (println "Reflecting existing user databases")
         (doseq [db-name (->> (d/get-database-names (str transactor-uri "*"))
                              (remove #(= root-transactor-uri (str transactor-uri %))))]
           (println (str "Registering db: " db-name))
           @(d/transact root-conn [(generate-db-record db-name)]))

         (println "Finished initializing"))))))

(defn init-from-file
  ([transactor-uri hf-schema-src]
   (assert (.exists (io/file hf-schema-src)))
   (init-from-file transactor-uri hf-schema-src nil))
  ([transactor-uri hf-schema-src hf-data-src]
   (assert (.exists (io/file hf-schema-src)))
   (init transactor-uri
         (-> hf-schema-src slurp read-string)
         (some-> hf-data-src slurp read-string))))
