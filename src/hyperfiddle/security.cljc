(ns hyperfiddle.security
  (:require
    #?(:clj [hyperfiddle.io.datomic])
    #?(:clj [hyperfiddle.domain])
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (hypercrud.types.DbRef DbRef)
       (datomic.impl Exceptions$IllegalArgumentExceptionInfo))))


(def root "hyperfiddle.security/root")                      ; todo uuid/real account

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

(defn write-allow-anonymous [$ domain dbname subject tx]
  tx)

(defn write-authenticated-users-only [$ domain dbname subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    tx))

(defn write-owner-only [$ domain dbname subject tx]
  (if (-> (into #{root} (:hyperfiddle/owners (hyperfiddle.domain/database domain dbname)))
          (contains? subject))
    tx
    (throw (tx-validation-failure))))

(defn tx-forbidden-attrs [whitelist tx]
  (let [tx-as (map (fn [[o e a v :as stmt]]
                     a)
                   tx)
        #_#_is-valid (clojure.set/superset? attr-whitelist tx-as)]
    (clojure.set/difference (set tx-as) (set whitelist))))

(defn attr-whitelist-query [$]
  {:query '[:find [?ident ...] :where
            [:db.part/db :db.install/attribute ?attr]
            [?attr :db/ident ?ident]

            ; this attribute might not be installed, they have to add it manually at stage
            ; If not installed, the query throws
            [?attr :hyperfiddle/whitelist-attribute true]]
   :args [$]
   :limit -1
   #_#_:offset nil})

#?(:clj
   (defn whitelist [$                                       ; Already has the proper staging area applied (one prior to the tx currently being validated)
                    domain db-name]                         ; spaghetti params used to decide which Datomic native q function to call
     {:pre [$ domain db-name]}
     ; TODO this is IO for non-local datomic configurations
     (let [whitelist (try
                       ((hyperfiddle.io.datomic/qf domain [(let [branch nil] ; qf ignores branch
                                                             (DbRef. db-name branch))])
                        (attr-whitelist-query $))
                       (catch Exception e
                         ; datomic.impl.Exceptions$IllegalArgumentExceptionInfo:
                         ; :db.error/not-an-entity Unable to resolve entity: :hyperfiddle/whitelist-attribute
                         nil))]

       ; Make sure they can add :hyperfiddle/whitelist-attr schema attribute the first time
       ; through the staging area (otherwise would have to do it out of band)
       (-> (set whitelist)
           (conj :db/ident :db/valueType :db/unique :db/cardinality)))))

#?(:clj
   (defn tx-attrs-pass-whitelist? [domain dbname $ subject tx]
     ; pass the $ or pass the schema? passing the schema is faster in the client case to optimize that query
     ; The spaghetti code upstack is too much, just do the query
     (let [whitelist (whitelist $ domain dbname)
           forbidden-attrs (tx-forbidden-attrs whitelist tx)
           is-pass (empty? forbidden-attrs)]
       #_(timbre/debug "database whitelist" whitelist)
       #_(timbre/debug "validating tx" (pr-str tx))
       #_(timbre/debug "passed attr whitelist?" is-pass "forbidden attrs: " forbidden-attrs)
       is-pass)))

#?(:clj
   (defn attr-whitelist! [$ domain dbname subject tx]       ; new param
     (let [whitelist (conj (whitelist $ domain dbname) :swing/whitelist-attribute)
           forbidden-as (tx-forbidden-attrs whitelist tx)]
       (if (empty? forbidden-as)
         tx
         (throw (let [msg (str "attributes: " (pr-str forbidden-as) " not on writable attribute whitelist: " (pr-str whitelist))]
                  (ex-info msg {:hf/anomoly msg
                                :hf/forbidden-attributes forbidden-as
                                :hf/whitelisted-attributes whitelist})))))))
