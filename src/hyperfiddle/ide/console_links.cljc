(ns hyperfiddle.ide.console-links
  (:require
    [cats.core :refer [mlet =<< >>= fmap return]]
    [clojure.core.match :refer [match #?(:cljs match*)]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.data]
    [contrib.datomic :refer [valueType ref? pull-shape pull-enclosure]]
    [contrib.reactive :as r]
    [contrib.reader :refer [memoized-read-edn-string+]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [datascript.parser :as parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.browser.context]
    [hyperfiddle.ide.system-fiddle :as system-fiddle]
    [hyperfiddle.fiddle :as fiddle])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(defn ^:export system-link? [link-id]                       ; is this still a thing?
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))

(defn path->str [path]
  (blank->nil (string/join " " path)))

(def console-fiddle-ctors
  {}
  #_{nil system-fiddle/console-self
   :hf/new system-fiddle/console-new
   :hf/remove nil})

(defn console-link [qfind [rel path]]
  (let [ix (condp = (type qfind)
             FindColl 0
             FindScalar 0
             FindRel (first path)
             FindTuple (first path))
        source (get-in (parser/find-elements qfind) [ix :source :symbol])]
    {:db/id (keyword "hyperfiddle.browser.system-link" (str (name rel) "-" (hash path)))
     :link/class #{rel}
     :link/path (path->str path)                            ; Explicit fe-pos where sometimes it can be implied
     :link/fiddle (some-> (console-fiddle-ctors rel) (apply [source]))}))

(defn console-links-rules [schema qfind e path]
  ; core.match gotcha: if you forget to declare a local binding, it will
  ; match as a symbol (wildcard)
  (let [FindColl FindColl
        FindScalar FindScalar
        FindRel FindRel
        FindTuple FindTuple
        Pull Pull
        Variable Variable
        Aggregate Aggregate]
    (let [?attr (last path)]
      (#?(:cljs match* :clj match)

        [(type qfind) (type e) ?attr (some->> ?attr (valueType schema))]
        ; nil attr means the pulled entity at the root of pulledtree

        ; Only findcoll gets hf/new at root, because if there is >1 findelement it won't show up with the relation
        [(:or FindColl) Pull nil _] #{nil :hf/remove :hf/new}
        [(:or FindScalar FindRel FindTuple) Pull nil _] #{nil :hf/remove}

        ; Descendents are the same
        [_ Pull _ :db.type/ref] #{nil :hf/new :hf/remove}

        [_ Variable _ _] nil
        [_ Aggregate _ _] nil
        [_ _ _ _] nil
        ))))

(defn console-links-e [schemas qfind ix e collection]
  (let [schema (get schemas (str (get-in e [:source :symbol])))
        all-paths (contrib.datomic/element-spread schema e collection)
        all-links (->> all-paths (map (partial console-links-rules schema qfind e)))
        all-paths (->> all-paths (map (fn [path]
                                        (condp = (type qfind)
                                          FindColl path
                                          FindScalar path
                                          FindRel (cons ix path)
                                          FindTuple (cons ix path)))))]
    ; These are canonical paths, which elide find-element index where it can be implied
    (->> (map vector all-paths all-links)
         (remove (comp nil? second)))))

(defn query-links-impl [schemas qfind result]
  (contrib.datomic/spread-elements console-links-e schemas qfind result))

(defn query-links "repl friendly interface" [schemas q data]
  (some-> (try-either (:qfind (parser/parse-query q)))
          (->> (unwrap println))
          (as-> qfind (query-links-impl schemas qfind data)))) ; (map (partial console-link source))

(defn console-links-fiddle
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [schemas fiddle data]
  (if-let [{:keys [:qfind]} (hyperfiddle.fiddle/parse-fiddle-query fiddle)]
    (map (comp fiddle/auto-link console-link)
         (repeat qfind)
         (contrib.data/ungroup (query-links-impl schemas qfind data)))))
