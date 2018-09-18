(ns hyperfiddle.security.domains
  (:require
    [hyperfiddle.security :as security]
    #?(:clj
    [hyperfiddle.security.entity-ownership :as entity-ownership])))

#?(:clj
   (def server
     {::security/process-tx entity-ownership/write-domains}))

(def client hyperfiddle.security.client/entity-ownership)
