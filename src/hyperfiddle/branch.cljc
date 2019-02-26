(ns hyperfiddle.branch)


(defn parent-branch-id [branch-id]
  {:pre [(vector? branch-id)]
   :post (vector? %)}
  (vec (drop-last branch-id)))

(defn child-branch-id [parent-branch-id relative-id]
  {:pre [(vector? parent-branch-id)]
   :post (vector? %)}
  (conj parent-branch-id relative-id))

(defn root-branch? [branch-id]
  {:pre [(vector? branch-id)]}
  (= 1 (count branch-id)))
