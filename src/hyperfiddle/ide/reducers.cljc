(ns hyperfiddle.ide.reducers)


(defn user-profile-reducer [user-profile action & args]
  (case action
    :set-user-profile (first args)
    user-profile))

(def reducer-map {:user-profile user-profile-reducer})
