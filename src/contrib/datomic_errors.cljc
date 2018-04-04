(ns contrib.datomic-errors)


(defn parse-datomic-error-soup [e]
  (or
    (if-let [[match a b] (re-find #"^(.+ :db.error/insufficient-binding )(.+)$" e)] [:db.error/insufficient-binding b])
    (if-let [[match a b] (re-find #"^(.+ :db.error/not-an-entity )(.+)$" e)] [:db.error/not-an-entity b])
    (if-let [[match a b] (re-find #"^(.+ :hyperfiddle.error/basis-stale )(.+)$" e)] [:hyperfiddle.error/basis-stale b])
    ))

(defn datomic-error-cleaner [error-str]
  (let [[title data] (if-let [[ident human] (parse-datomic-error-soup error-str)]
                       [(str ident) {:ident ident :human human}]
                       ["Unrecognized Datomic error" {:ident :hyperfiddle.error/unrecognized :soup error-str}])]
    (ex-info title data)))