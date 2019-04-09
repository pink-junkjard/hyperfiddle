(ns hyperfiddle.ide.serialization
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :refer :all]
    [clojure.walk :as walk]
    [contrib.datomic-peer :as datomic-peer]
    [contrib.reader :as reader]
    [datomic.api :as d]
    [taoensso.timbre :as timbre])
  (:import
    (java.util Date)))


(defn from-db [db]
  (let [project (-> (d/pull db ['*] :hyperfiddle/project)
                    (dissoc :db/id))
        renderers (->> (d/q '[:find [(pull ?e [:attribute/ident :attribute/renderer]) ...]
                              :where [?e :attribute/renderer]] db)
                       (sort-by :attribute/ident))
        fiddles (->> (d/q '[:find [?fiddle ...] :where [?fiddle :fiddle/ident]] db)
                     (datomic-peer/clone-entities db)
                     (walk/postwalk (fn [o] (cond
                                              (map? o) (apply sorted-map (apply concat o))
                                              (map-entry? o) o
                                              (or (set? o) (vector? o)) (vec (sort-by :link/path o))
                                              :else o)))
                     (sort-by :fiddle/ident))]
    (vec (concat
           [project]
           renderers
           fiddles))

    ; todo just export based on schema
    ; need to solve for child component entities with clone-entities (e.g. links repeat)
    #_(->> (io/resource "schema/fiddle.edn") slurp reader/read-edn-string!
           (map :db/ident)
           (remove #(= "link" (namespace %)))
           (d/q '[:find [?e ...] :in db [?ident ...]
                  :where
                  [?a :db/ident ?ident]
                  [?e ?a _]]
                db)
           set
           (datomic-peer/clone-entities db))
    ))

(defn mandatory-newlines-dispatch [o]
  (cond
    (map? o) (let [amap o
                   [ns lift-map] (when (not (record? amap))
                                   (#'clojure.core/lift-ns amap))
                   amap (or lift-map amap)
                   prefix (if ns (str "#:" ns "{") "{")]
               (pprint-logical-block :prefix prefix :suffix "}"
                                     (print-length-loop [aseq (seq amap)]
                                                        (when aseq
                                                          (pprint-logical-block
                                                            (write-out (ffirst aseq))
                                                            (.write ^java.io.Writer *out* " ")
                                                            #_(pprint-newline :linear)
                                                            (var-set #'clojure.pprint/*current-length* 0) ; always print both parts of the [k v] pair
                                                            (write-out (fnext (first aseq))))
                                                          (when (next aseq)
                                                            #_(.write ^java.io.Writer *out* ", ")
                                                            (pprint-newline :mandatory)
                                                            (recur (next aseq)))))))
    (vector? o) (let [avec o]
                  (pprint-logical-block :prefix "[" :suffix "]"
                                        (print-length-loop [aseq (seq avec)]
                                                           (when aseq
                                                             (write-out (first aseq))
                                                             (when (next aseq)
                                                               #_(.write ^java.io.Writer *out* " ")
                                                               (pprint-newline :mandatory)
                                                               (recur (next aseq)))))))
    :else (clojure.pprint/simple-dispatch o)))

(defn generate-edn-str [$]
  (let [w "; AUTO-GENERATED FILE - DO NOT EDIT BY HAND\n"]
    (str
      w w
      ";\n"
      "; generated at: " (Date.) "\n"
      "; basis: " (d/basis-t $) "\n"
      ";\n"
      w w "\n\n"
      (binding [clojure.pprint/*print-pprint-dispatch* mandatory-newlines-dispatch]
        (with-out-str (clojure.pprint/pprint (from-db $)))))))

; from cloud-jvm repl:
;   (hyperfiddle.ide.serialization/write-edn! (db! root-uri) "../ide/resources/ide.edn")
(defn write-edn! [$ output-file]
  (->> (generate-edn-str $)
       (spit output-file)))

(defn transact-ide! [uri]
  (let [ide-schema (-> (io/resource "schema/fiddle.edn") slurp reader/read-edn-string!)
        ide-data (-> (io/resource "ide.edn") slurp reader/read-edn-string!)]
    @(d/transact (d/connect (str uri)) ide-schema)
    @(d/transact (d/connect (str uri)) ide-data)))

(defn initialize-ide! [uri]
  (if (d/create-database (str uri))
    (transact-ide! uri)
    (timbre/warnf "Database already exists for uri '%s'. Skipping data load." uri)))
