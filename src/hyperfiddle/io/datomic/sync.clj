(ns hyperfiddle.io.datomic.sync
  (:refer-clojure :exclude [sync])
  (:require
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.datomic :as d]
    [taoensso.timbre :as timbre]))


(defn sync [datomic domain dbnames]
  (timbre/debug "syncing" (pr-str dbnames))
  (->> dbnames
       (map (juxt identity #(->> (domain/database domain %)
                                 (d/connect datomic)
                                 (d/basis datomic))))
       (into {})))
