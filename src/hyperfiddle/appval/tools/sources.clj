(ns hyperfiddle.appval.tools.sources
  (:require [clojure.string :as string]
            [datomic.api :as d]))


(defn code-search [word conn]
  (let [db (d/db conn)]
    (d/q '[:find ?e ?ident ?v :in $ ?word :where
           [?a :db/ident ?ident]
           [?a :db/valueType :db.type/string]
           [(.contains ?v ?word)]
           [$ ?e ?a ?v]]
         db word)))

(defn code-replace [match replacement conn]
  (->> (code-search match conn)
       (mapv (fn [[e a v]]
               {:db/id e
                a (string/replace v match replacement)}))
       (apply vector)))

; an odd utility, finds words in keywords (used for fixing link/rels, which have embeded idents within keywords)
(defn keyword-search [word conn]
  (let [db (d/db conn)]
    (d/q '[:find ?e ?ident ?v :in $ ?word :where
           [?a :db/ident ?ident]
           [?a :db/valueType :db.type/keyword]
           [(str ?v) ?str-kw]
           [(.contains ?str-kw ?word)]
           [$ ?e ?a ?v]]
         db word)))

; an odd utility, replaces words in keywords (used for fixing link/rels, which have embeded idents within keywords)
(defn keyword-replace [match replacement conn]
  (->> (keyword-search match conn)
       (mapv (fn [[e a v]]
               {:db/id e
                a (-> (string/replace (str v) match replacement)
                      (subs 1)
                      (keyword))}))
       (apply vector)))

; this only finds referenced code-databases
; if a database exists that isn't used anywhere, it won't be found
(defn list-used-source-db-uris []
  (d/q '[:find [?uri ...] :in $ :where
         [?domain :domain/code-databases ?cdb]
         [?cdb :dbhole/uri ?uri]]
       (d/db (d/connect "datomic:free://datomic:4334/domains"))))

(defn with-source-code-conns [f]
  (->> (list-used-source-db-uris)
       (map (juxt identity #(f (d/connect (str %)))))
       (remove #(or (empty? (second %))
                    (nil? (second %))))
       (into {})))

(defn code-search-sources [word]
  (with-source-code-conns #(code-search word %)))

(defn code-replace-sources [match replacement]
  (with-source-code-conns #(code-replace match replacement %)))

(defn all-source-vals [ident]
  (with-source-code-conns
    (fn [conn]
      (->> (d/q '[:find ?e ?v :in $ ?a :where [?e ?a ?v]] (d/db conn) ident)
           (into {})))))

(defn find-dead-fiddles [conn]
  (let [$ (d/db conn)]
    (d/q '[:find [?fiddle ...] :in $ :where
           (not [_ :link/fiddle ?fiddle])
           (or [?fiddle :fiddle/name]
               [?fiddle :fiddle/type])]
         $)))

(defn unnamed-fiddles [conn]
  (let [$ (d/db conn)]
    (d/q '[:find [?fiddle ...] :in $ :where
           (not [?fiddle :fiddle/name])
           [?fiddle :fiddle/type]]
         $)))

(defn all-query-values [source-conn]
  (let [$ (d/db source-conn)]
    (d/q '[:find [?value ...] :in $ :where
           [_ :fiddle/query ?value]]
         $)))
