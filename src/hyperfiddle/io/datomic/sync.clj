(ns hyperfiddle.io.datomic.sync
  (:refer-clojure :exclude [sync])
  (:require
    [datomic.api :as d]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.datomic.api :as hd]
    [taoensso.timbre :as timbre]))


(defn sync [domain dbnames]
  (timbre/debug "syncing" (pr-str dbnames))
  (reduce
    (fn [acc dbname]
      (let [uri (:database/uri (domain/database domain dbname))]
        (assoc acc dbname (-> (str uri) hd/connect d/sync deref d/basis-t))))
    {}
    dbnames))
