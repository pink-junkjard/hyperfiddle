(ns hyperfiddle.ide.util)


(defn owner-fn [ctx]
  (let [owner (:owner ctx)
        current-user (-> ctx :user-profile :sub)
        owned-by-nobody (nil? owner)                        ; explicitly the nil case, not the empty-set case, which means non-editable
        owned-by-me (contains? (set owner) current-user)]
    (or owned-by-me owned-by-nobody)))
