(ns contrib.datomic-errors
  (:require [contrib.data :refer [cond-let parse-query-element]]
            [cuerdas.core :as string]))


(defn parse-datomic-error-soup [e req]
  (cond-let
    [msg #_(re-find #"(?s)^.+ :db.error/datoms-conflict (.+)$" e)
     (some-> (string/split e #"^.+ :db\.error/datoms-conflict ") second)]
    [:db.error/datoms-conflict msg
     "Hint: Hyperfiddle has generated an invalid Datomic transaction. If you are using the staging area, this is
     probably a 'merge conflict' which must be reconciled by editing the staging area by hand. If it is a buggy
     widget, please file an issue."]
    [[match msg] (re-find #"^.+ :db.error/invalid-entity-id (.+)$" e)] [:db.error/invalid-entity-id msg]
    [[match msg] (re-find #"^.+ :db.error/insufficient-binding (.+)$" e)]
    [:db.error/insufficient-binding msg
     (str (some-> req .-query pr-str) "\n\n" "Hint: Click 'src' to see and edit the query.")]
    [[match msg] (re-find #"^.+ :db.error/not-a-data-function (.+)$" e)] [:db.error/not-a-data-function msg]
    [[match msg] (re-find #"^.+ :db.error/not-an-entity (.+)$" e)]
    [:db.error/not-an-entity msg
     "Hint: If this is a schema attribute, does it exist?
     This can happen if you both create a schema entity and use it in the
     same transaction. Transact the schema before using it. Also try auto-transact."]
    [[match msg] (re-find #"^.+ :db.error/wrong-type-for-attribute (.+)$" e)] [:db.error/wrong-type-for-attribute msg]
    [[match msg] (re-find #"^.+ :hyperfiddle.error/basis-stale (.+)$" e)] [:hyperfiddle.error/basis-stale msg]

    ; It is the Clojure way.
    [[match msg] (re-find #"^com.google.common.util.concurrent.UncheckedExecutionException: java.lang.IllegalArgumentException: (.+)$" e)] [:hyperfiddle.error/invalid-pull msg]
    [[match msg] (re-find #"^.+ message: Unable to find data source: (.+)$" e)]
    (let [expected (parse-query-element (some-> req .-query) :in)]
      [:hyperfiddle.error/query-arity
       (str "Query argument missing: " msg " which corresponds with " expected ".\n"
            "Hint: add a link/formula or edit the URL.")])))

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
