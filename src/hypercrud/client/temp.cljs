(ns hypercrud.client.temp
  (:require [clojure.set :as set]
            [hypercrud.types.DbId :refer [->DbId]]))


(defn id->tempid [id-lookup id]
  (get id-lookup id id))

(defn dbid->tempdbid [id-lookup dbid]
  (->DbId (id->tempid id-lookup (:id dbid)) (:uri dbid)))

(defn tempid->id [id-lookup id]
  (get (set/map-invert id-lookup) id id))

(defn tempdbid->dbid [id-lookup dbid]
  (->DbId (tempid->id id-lookup (:id dbid)) (:uri dbid)))
