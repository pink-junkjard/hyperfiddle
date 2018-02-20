(ns hypercrud.util.branch
  (:require [clojure.string :as str]
            [hypercrud.types.DbVal :refer [#?(:cljs DbVal)]]
            [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
            [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)]])
  #?(:clj
     (:import (hypercrud.types.DbVal DbVal)
              (hypercrud.types.EntityRequest EntityRequest)
              (hypercrud.types.QueryRequest QueryRequest))))


(defn decode-parent-branch [branch]
  (let [parent (second (re-find #"(.*)`.*" branch))]
    (if (str/blank? parent)
      nil
      parent)))

(defn encode-branch-child [parent-branch child-id-str]
  (if (str/blank? child-id-str)
    parent-branch
    (str parent-branch "`" child-id-str)))

(defn get-all-branches [branch]
  (if branch
    (conj (get-all-branches (decode-parent-branch branch)) branch)
    [branch]))

(defn branch-val [uri branch stage-val]
  (->> (get-all-branches branch)
       (reduce (fn [tx branch]
                 (concat tx (get-in stage-val [branch uri])))
               nil)
       hash))

(defn request->dbvals [request]
  (condp = (type request)
    EntityRequest [(:db request)]
    QueryRequest (filter #(= (type %) DbVal) (:params request))))

(defn branch-vals-for-dbvals [dbvals stage-val]
  (->> dbvals
       (map (fn [db] (branch-val (:uri db) (:branch db) stage-val)))
       (hash)))
