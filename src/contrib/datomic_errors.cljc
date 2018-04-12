(ns contrib.datomic-errors)


(defn parse-datomic-error-soup [e]
  (or
    (if-let [[match a b] (re-find #"^(.+ :db.error/datoms-conflict )(.+)$" e)] [:db.error/datoms-conflict b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/invalid-entity-id )(.+)$" e)] [:db.error/invalid-entity-id b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/insufficient-binding )(.+)$" e)] [:db.error/insufficient-binding b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/not-a-data-function )(.+)$" e)] [:db.error/not-a-data-function b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/not-an-entity )(.+)$" e)] [:db.error/not-an-entity b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/wrong-type-for-attribute )(.+)$" e)] [:db.error/wrong-type-for-attribute b])
    (if-let [[match a b] (re-find #"^(.+ :hyperfiddle.error/basis-stale )(.+)$" e)] [:hyperfiddle.error/basis-stale b])

    ; It is the Clojure way.
    (if-let [[match a b] (re-find #"^(com.google.common.util.concurrent.UncheckedExecutionException: java.lang.IllegalArgumentException: )(.+)$" e)] [:hyperfiddle.error/invalid-pull b])
    (if-let [[match a b] (re-find #"^(.+ message: Unable to find data source: )(.+)$" e)] [:hyperfiddle.error/query-arity (str "message: Unable to find data source: " b)])
    ))

(defn datomic-error-cleaner [error-str]
  (let [[title data] (if-let [[ident human] (parse-datomic-error-soup error-str)]
                       [(str ident) {:ident ident :human human}]
                       ["Unrecognized Datomic error" {:ident :hyperfiddle.error/unrecognized :soup error-str}])]
    (ex-info title data)))
