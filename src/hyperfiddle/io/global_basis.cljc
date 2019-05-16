(ns hyperfiddle.io.global-basis
  (:require
    [cats.core :as cats]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [taoensso.timbre :as timbre]))


(defn global-basis [io domain]
  (perf/time-promise
    (->> (domain/databases domain) keys set
         (io/sync io)
         (cats/fmap (fn [user-basis] {:domain {:t (domain/basis domain)
                                               :hash (hash {:fiddle-dbname (domain/fiddle-dbname domain)
                                                            :databases (->> (domain/databases domain)
                                                                            (map (fn [[dbname database]]
                                                                                   ; todo switching between peer/client will break this hash
                                                                                   [dbname (select-keys database [:database/uri :database/db-name])]))
                                                                            (into {}))
                                                            :environment (domain/environment domain)
                                                            :type-name (domain/type-name domain)})}
                                      :user user-basis})))
    (fn [err total-time]
      (timbre/debugf "global-basis failure;" "total time: %sms" total-time))
    (fn [success total-time]
      (timbre/debugf "global-basis;" "total time: %sms" total-time))))
