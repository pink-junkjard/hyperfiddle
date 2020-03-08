(ns hyperfiddle.security
  (:require
    #?(:clj [hyperfiddle.io.datomic])
    #?(:clj [hyperfiddle.domain])
    #?(:clj [hyperfiddle.schema])
    [contrib.datomic-tx :refer [remove-tx]]
    [contrib.pprint :refer [pprint-str pprint-datoms-str]]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (hypercrud.types.DbRef DbRef))))


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

(defn tx-forbidden-stmts [schema whitelist tx]
  (->> tx
       (remove-tx schema                                    ; handles map-form transactions and tx fns
                  (fn [[o & [e a v :as args] :as stmt]]
                    (condp some [o]
                      #{:db/add :db/retract} (contains? whitelist a) ; typical edits are secured by attribute
                      (contains? whitelist o))))))          ; transaction functions are secured by txfn name

(defn attr-whitelist-query [$]
  {:query '[:find [?ident ...] :where
            [?attr :db/ident ?ident]                        ; transactor functions have idents
            #_[:db.part/db :db.install/attribute ?attr]     ; transactor functions are not installed
            [?attr :hyperfiddle/whitelist-attribute true]]  ; If hf attr not installed, this clause throws
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
           (conj :db/ident :db/valueType :db/unique :db/cardinality
                 :hyperfiddle/whitelist-attribute           ; this is allowed by default, otherwise you can only set it true out-of-band
                 )))))

#?(:clj
   (defn tx-permitted? [domain dbname $ subject tx]
     ; pass the $ or pass the schema? passing the schema is faster in the client case to optimize that query
     ; The spaghetti code upstack is too much, just do the query
     (let [whitelist (whitelist $ domain dbname)
           schema (hyperfiddle.schema/-summon-schema-out-of-band domain dbname $)
           forbidden-stmts (tx-forbidden-stmts schema whitelist tx)
           is-pass (empty? forbidden-stmts)]
       #_(timbre/debug "database whitelist" whitelist)
       #_(timbre/debug "validating tx" (pr-str tx))
       #_(timbre/debug "passed attr whitelist?" is-pass "forbidden attrs: " forbidden-stmts)
       is-pass)))

#?(:clj
   (defn tx-operation-whitelist! [$ domain dbname subject tx]
     (let [whitelist (whitelist $ domain dbname)
           schema (hyperfiddle.schema/-summon-schema-out-of-band domain dbname $)
           forbidden-stmts (tx-forbidden-stmts schema whitelist tx)]
       (timbre/debug "forbidden statements: " forbidden-stmts)
       (if (empty? forbidden-stmts)
         tx
         (throw (let [msg (str "Forbidden statements: \n\n"
                               (pprint-datoms-str forbidden-stmts) "\n\n"
                               "are forbidden due to transaction whitelist: \n\n"
                               (pprint-str whitelist))]
                  (ex-info msg {}  #_{:hf/anomoly msg
                                :hf/forbidden-statements forbidden-stmts
                                :hf/transaction-whitelist whitelist})))))))
