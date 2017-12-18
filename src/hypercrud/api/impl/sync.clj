(ns hypercrud.api.impl.sync
  (:refer-clojure :exclude [sync])
  (:require [cuerdas.core :as str]
            [datomic.api :as d]))


(defn sync [dbs]                                            ; sync is the low level datomic call
  ; ordered kv seq
  (->> dbs (map (juxt identity #(-> % str d/connect d/sync deref d/basis-t)))))
