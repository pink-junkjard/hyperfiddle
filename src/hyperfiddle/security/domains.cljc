(ns hyperfiddle.security.domains
  (:require
    [hyperfiddle.security :as security]
    [hyperfiddle.security.client :as client-sec]
    #?(:clj
    [hyperfiddle.security.entity-ownership :as entity-ownership])))

#?(:clj
   (def server
     {:process-tx entity-ownership/write-domains}))

(def client client-sec/entity-ownership)
