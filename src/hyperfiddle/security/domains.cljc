(ns hyperfiddle.security.domains
  (:require
    [hyperfiddle.security :as security]
    #?(:clj
    [hyperfiddle.security.entity-ownership :as entity-ownership])))

#?(:clj
   (def server
     {::security/process-tx entity-ownership/write-domains}))

(def client
  {::security/subject-can-transact? (fn [hf-db subject] (not (nil? subject)))
   ::security/writable-entity? (fn [hf-db subject m]
                                 (and (not (nil? subject))
                                      (or (contains? (set (:hyperfiddle/owners hf-db)) subject)
                                          (contains? (set (:hyperfiddle/owners m)) subject))))})
