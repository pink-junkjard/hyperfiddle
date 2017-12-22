(ns hypercrud.api.impl.sync
  (:refer-clojure :exclude [sync])
  (:require [datomic.api :as d]
            [taoensso.timbre :as timbre]))


(defn sync [dbs]                                            ; sync is the low level datomic call
  (timbre/debug "syncing" (pr-str dbs))
  ; ordered kv seq
  (->> dbs
       (mapcat (juxt identity #(-> % str d/connect d/sync deref d/basis-t)))
       (apply sorted-map)))
