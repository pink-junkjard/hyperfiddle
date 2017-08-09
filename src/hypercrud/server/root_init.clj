(ns hypercrud.server.root-init
  (:require [clojure.java.io :as io]
            [datomic.api :as d]
            [hypercrud.util.core :as util]))


(defn generate-db-record [db-name]
  {:db/id (d/tempid :db.part/user)
   :database/ident db-name})

(defn reflect-schema [conn]
  (let [$ (d/db conn)]
    (->> (d/q '[:find [?a ...] :in $ :where [?a :db/ident]] $)
         (mapv #(d/touch (d/entity $ %)))
         ;filter out datomic attributes, todo this is a huge hack
         (filter #(> (:db/id %) 62)))))

(defn attr->hc-attr [attr]
  (->> attr
       (into {})
       (util/map-keys (fn [ident]
                        (condp = ident
                          :db/id ident
                          (keyword "attribute" (name ident)))))))

; root isn't parameterized that well
; the current hf-src will create a database for root (/root and the entity-id for root) AND pink (HFHF) AND user
; this means 4 databases on top of existing infra :(
(defn init [root-db-uri user-transactor-uri hf-schema-src hf-data-src]
  (if-not (d/create-database root-db-uri)
    (throw (new Error "Cannot initialize from existing database"))

    (let [root-conn (d/connect root-db-uri)]
      (println "Installing schema")
      @(d/transact root-conn (-> (io/resource hf-schema-src) slurp read-string))

      (println "Installing hyperfiddle sources")
      @(d/transact root-conn (-> (io/resource hf-data-src) slurp read-string))

      ; all of the following reflection shouldn't be necessary after removing the root
      ; user fiddles should execute it on demand
      (println "Reflecting existing user databases")
      (let [db-names (->> (d/get-database-names (str user-transactor-uri "*"))
                          (remove #(= root-db-uri (str user-transactor-uri %))))]

        (println "Registering user databases")
        @(d/transact root-conn (mapv generate-db-record db-names))

        (println "Reflecting user schemas")
        ; this will fail if attributes already exist
        @(d/transact root-conn (->> db-names
                                    (mapcat #(reflect-schema (d/connect (str user-transactor-uri %))))
                                    (mapv attr->hc-attr)
                                    (into #{})
                                    (into []))))

      (println "Finished"))))
