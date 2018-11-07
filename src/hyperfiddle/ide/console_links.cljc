(ns hyperfiddle.ide.console-links
  (:require
    [cats.core :refer [mlet =<< >>= fmap return]]
    [clojure.core.match :refer [match #?(:cljs match*)]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [merge-by transpose pad ungroup]]
    [contrib.datomic :refer [valueType ref? pull-shape enclosing-pull-shape pull-traverse]]
    [contrib.reactive :as r]
    [contrib.reader :refer [memoized-read-edn-string+]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [datascript.parser :as parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hyperfiddle.ide.system-fiddle :as system-fiddle]
    [hyperfiddle.fiddle :as fiddle])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(defn ^:export system-link? [link-id]                       ; is this still a thing?
  (and (keyword? link-id)
       (some-> (namespace link-id) (string/starts-with? "hyperfiddle.browser.system-link"))))

(defn path->str [path]
  (blank->nil (string/join " " path)))

(def console-fiddle-ctors
  {:hf/detach nil
   :hf/remove nil
   :hf/self system-fiddle/console-self
   :hf/new system-fiddle/console-new
   :hf/affix system-fiddle/console-affix})

(defn console-link [qfind [rel path]]
  (let [ix (condp = (type qfind)
             FindColl 0
             FindScalar 0
             FindRel (first path)
             FindTuple (first path))
        source (get-in (parser/find-elements qfind) [ix :source :symbol])]
    {:db/id (keyword "hyperfiddle.browser.system-link" (str (name rel) "-" (hash path)))
     :link/rel rel
     :link/path (path->str path)                            ; Explicit fe-pos where sometimes it can be implied
     :link/fiddle (some-> (console-fiddle-ctors rel) (apply [source]))}))

(defn element-spread [schema {{pull-pattern :value} :pattern :as e} collection]
  (condp = (type e)
    Pull (pull-traverse (enclosing-pull-shape schema (pull-shape pull-pattern) collection))
    Variable [[]]
    Aggregate [[]]))

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

        [(type qfind) (type e) ?attr (valueType schema ?attr)]
        ; nil attr means the pulled entity at the root of pulledtree

        ; Only findcoll gets hf/new at root, because if there is >1 findelement it won't show up with the relation
        [(:or FindColl) Pull nil _] #{:hf/self :hf/remove :hf/new}
        [(:or FindScalar FindRel FindTuple) Pull nil _] #{:hf/self :hf/remove}

        ; Descendents are the same
        [_ Pull _ :db.type/ref] #{:hf/self :hf/affix :hf/detach}

        [_ Variable _ _] nil
        [_ Aggregate _ _] nil
        [_ _ _ _] nil
        ))))

(defn console-links-e [schemas qfind ix e collection]
  (let [schema @(get schemas (str (get-in e [:source :symbol])))
        all-paths (element-spread schema e collection)
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

(defn normalize-result [qfind result]
  (condp = (type qfind)
    FindColl (mapv vector result)
    FindRel result
    FindTuple (mapv vector result)
    FindScalar [[result]]))

(defn query-links-impl [schemas qfind result]
  (mapcat (partial console-links-e schemas qfind)           ; (map (partial console-link source)) % has the source
          (range)                                           ; find-element index, for source reversing
          (parser/find-elements qfind)
          (pad nil (transpose (normalize-result qfind result)))))

(defn query-links "repl friendly interface" [schemas q data]
  (some-> (try-either (:qfind (parser/parse-query q)))
          (->> (unwrap println))
          (as-> qfind (query-links-impl schemas qfind data)))) ; (map (partial console-link source))

(defn console-links-fiddle
  "All sys links can be matched and merged with user-links. Matching is determined by link/rel and link/path"
  [schemas {:keys [fiddle/type fiddle/query fiddle/pull fiddle/pull-database]} data]
  (let [qfind (->> (case type
                     :blank nil
                     :entity (->> (memoized-read-edn-string+ pull)
                                  (fmap (fn [pull]
                                          (let [source (symbol pull-database)
                                                fake-q `[:find (~'pull ~source ~'?e ~pull) . :where [~'?e]]]
                                            (parser/parse-query fake-q)))))
                     :query (->> (memoized-read-edn-string+ query)
                                 (=<< #(try-either (parser/parse-query %)))))
                   (unwrap (constantly nil))
                   :qfind)]
    (map (comp fiddle/auto-link console-link)
         (repeat qfind)
         (ungroup (query-links-impl schemas qfind data)))))

(let [f (fn [new-links fiddle]
          (update fiddle :fiddle/links (partial merge-by (juxt :link/rel (comp blank->nil :link/path)) new-links)))]
  (defn inject-console-links [ctx]
    (let [links (console-links-fiddle (:hypercrud.browser/schemas ctx) @(:hypercrud.browser/fiddle ctx) @(:hypercrud.browser/data ctx))]
      (update ctx :hypercrud.browser/fiddle #(r/fmap->> % (f links))))))