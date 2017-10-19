(ns hypercrud.client.temp
  (:require [hypercrud.types.DbId :refer [->DbId]]))


(defn dbid->tempdbid [id->tempid dbid]
  (->DbId (get id->tempid (:id dbid) (:id dbid))
          (:uri dbid)))

(defn tempdbid->dbid [tempid->id temp-dbid]
  (->DbId (get tempid->id (:id temp-dbid) (:id temp-dbid))
          (:uri temp-dbid)))
