(ns hyperfiddle.security)


(def root "hyperfiddle.security/root")                      ; todo uuid/real account

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

(defn write-allow-anonymous [hf-db subject tx]
  tx)

(defn write-authenticated-users-only [hf-db subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    tx))

(defn write-owner-only [hf-db subject tx]
  (if (-> (into #{root} (:hyperfiddle/owners hf-db))
          (contains? subject))
    tx
    (throw (tx-validation-failure))))
