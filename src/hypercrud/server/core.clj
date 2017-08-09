(ns hypercrud.server.core
  (:require [datomic.api :as d]
            [hypercrud.server.database :as database]
            [hypercrud.server.db-root :as db]
            [io.pedestal.http :as bootstrap]))


(defn init-datomic [transactor-uri]
  (let [root-uri (str transactor-uri "root")
        db-created? (d/create-database root-uri)]           ;idempotent
    (assert (not db-created?) "Must seed with a real root-db, can't bootstrap from nothing anymore.")

    (alter-var-root #'db/transactor-uri (constantly transactor-uri))
    (alter-var-root #'db/root-id (constantly
                                   (d/q '[:find ?db . :where [?db :database/ident "root"]]
                                        (d/db (database/get-root-conn)))))))

(defn start-service []
  )

