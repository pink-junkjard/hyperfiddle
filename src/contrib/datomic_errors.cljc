(ns contrib.datomic-errors
  (:require [contrib.data :refer [cond-let]]
            [cuerdas.core :as string]))


(defn parse-datomic-error-soup [e req]
  (cond-let
    [msg #_(re-find #"(?s)^.+ :db.error/datoms-conflict (.+)$" e)
     (some-> (string/split e #"^.+ :db\.error/datoms-conflict ") second)]
    [:db.error/datoms-conflict msg
     "Hyperfiddle has generated an invalid Datomic transaction ([hyperfiddle#24](https://github.com/hyperfiddle/hyperfiddle/issues/24)).
     Please repair it by hand in the staging area. If this happens too much,
     you can avoid this with auto-transact. Sometimes this is a \"merge conflict\"
     and essential complexity. Sometimes it is a buggy form widget."]
    [[match msg] (re-find #"^.+ :db.error/invalid-entity-id (.+)$" e)] [:db.error/invalid-entity-id msg]
    [[match msg] (re-find #"^.+ :db.error/insufficient-binding (.+)$" e)] [:db.error/insufficient-binding msg
                                                                           (str "Use 'src' view to fix the query." #_#_#_"\n```\n" (some-> req .-query pr-str) "\n```")]
    [[match msg] (re-find #"^.+ :db.error/not-a-data-function (.+)$" e)] [:db.error/not-a-data-function msg]
    [[match msg] (re-find #"^.+ :db.error/not-an-entity (.+)$" e)]
    [:db.error/not-an-entity msg
     "If this is a schema attribute, does it exist?
     This can happen if you both create a schema entity and use it in the
     same transaction. Transact the schema before using it. Also try auto-transact."]
    [[match msg] (re-find #"^.+ :db.error/wrong-type-for-attribute (.+)$" e)] [:db.error/wrong-type-for-attribute msg]
    [[match msg] (re-find #"^.+ :hyperfiddle.error/basis-stale (.+)$" e)] [:hyperfiddle.error/basis-stale msg]

    ; It is the Clojure way.
    [[match msg] (re-find #"^com.google.common.util.concurrent.UncheckedExecutionException: java.lang.IllegalArgumentException: (.+)$" e)] [:hyperfiddle.error/invalid-pull msg]
    [[match msg] (re-find #"^.+ message: Unable to find data source: (.+)$" e)] [:hyperfiddle.error/query-arity (str "message: Unable to find data source: " msg)]
    ))

(defn datomic-error-cleaner [error-str req]
  (let [[title data] (if-let [[ident error-msg human-hint] (parse-datomic-error-soup error-str req)]
                       [(str ident) {:ident ident
                                     :error-msg error-msg
                                     :human-hint human-hint}]

                       [(str :hyperfiddle.error/unrecognized)
                        {:ident :hyperfiddle.error/unrecognized
                         :error-msg error-str
                         :human-hint
                         (str "Please comment this error at [hyperfiddle#170](https://github.com/hyperfiddle/hyperfiddle/issues/170) so we can match it."
                              "\n\n```\n" (pr-str req) "\n```")}])]
    (ex-info title data)))
