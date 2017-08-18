(ns hypercrud.server.datomic.root-init
  (:require [clojure.java.io :as io]
            [datomic.api :as d]
            [hypercrud.util.core :as util]))


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

(defn attr->hc-attr [attr]
  (->> attr
       (into {})
       (util/map-keys (fn [ident]
                        (condp = ident
                          :db/id ident
                          (keyword "attribute" (name ident)))))))

; root isn't parameterized that well
; the current hf-src will create a database for root AND hyperfiddle AND hyperfiddle-users
; this means 3 databases on top of existing infra :(
(defn init [transactor-uri hf-schema-src hf-data-src]
  (assert (.exists (io/file hf-schema-src)))
  (assert (.exists (io/file hf-data-src)))
  (let [root-transactor-uri (str transactor-uri "root")]
    (if-not (d/create-database root-transactor-uri)
      (throw (new Error "Cannot initialize from existing database"))

      (let [root-conn (d/connect root-transactor-uri)]
        (println "Installing schema")
        @(d/transact root-conn (-> hf-schema-src slurp read-string))

        (println "Installing hyperfiddle sources")
        @(d/transact root-conn (-> hf-data-src slurp read-string))

        ; all of the following reflection shouldn't be necessary after removing the root
        ; user fiddles should execute it on demand
        (println "Reflecting existing user databases")
        (let [db-names (->> (d/get-database-names (str transactor-uri "*"))
                            (remove #(= root-transactor-uri (str transactor-uri %))))]

          (println "Registering user databases")
          @(d/transact root-conn (mapv generate-db-record db-names))

          (println "Reflecting user schemas")
          ; this will fail if attributes already exist
          @(d/transact root-conn (->> db-names
                                      (mapcat #(reflect-schema (d/connect (str transactor-uri %))))
                                      (mapv attr->hc-attr)
                                      (into #{})
                                      (into []))))

        (println "Finished")))))
