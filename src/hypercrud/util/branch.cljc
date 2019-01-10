(ns hypercrud.util.branch
  (:require
    [clojure.string :as string]
    [contrib.string :refer [blank->nil]]))


(defn decode-parent-branch "nil is a valid branch" [branch]
  (some->> branch (re-find #"(.*)`.*") second blank->nil))

(defn encode-branch-child [parent-branch child-id-str]
  (if (string/blank? child-id-str)
    parent-branch
    (str parent-branch "`" child-id-str)))

(defn get-all-branches [branch]
  (if branch
    (conj (get-all-branches (decode-parent-branch branch)) branch)
    [branch]))

(defn branch-val [dbname branch partitions]
  (->> (get-all-branches branch)
       (reduce (fn [tx branch]
                 (concat tx (get-in partitions [branch :stage dbname])))
               nil)
       hash))
