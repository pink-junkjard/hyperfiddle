(ns hyperfiddle.fiddle
  (:require
    [cats.core :as cats :refer [mlet return =<< fmap]]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [update-existing]]
    [contrib.reader]
    [contrib.string :refer [or-str]]
    [contrib.template :as template]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [datascript.parser]
    #_[hyperfiddle.ui]
    #_[hyperfiddle.api]                                     ; tempid formulas
    [taoensso.timbre :as timbre]))


(defmulti fiddle-type :fiddle/type)

(s/def :hyperfiddle/ide                                     ; !! fiddle/ident overlap with attributes
  (s/and
    #_(s/multi-spec fiddle-type :fiddle/type)
    #_(s/or :ident (s/keys :req [:fiddle/ident])
            :uuid (s/keys :req [:fiddle/uuid]))
    (s/keys :opt [:fiddle/ident
                  :fiddle/uuid
                  :fiddle/type
                  :fiddle/links
                  :fiddle/markdown
                  :fiddle/renderer
                  :fiddle/css
                  :fiddle/cljs-ns
                  :fiddle/hydrate-result-as-fiddle
                  :hyperfiddle/owners])))

(s/def :hyperfiddle.ide/new-fiddle (s/keys :req [:fiddle/ident]))

(s/def :fiddle/ident keyword?)
(s/def :fiddle/uuid uuid?)
(s/def :fiddle/type #{:blank :entity :query})
(s/def :fiddle/query string?)
(s/def :fiddle/pull string?)
(s/def :fiddle/pull-database string?)
(s/def :fiddle/links (s/coll-of (s/and (s/keys :opt [:link/class
                                                     :link/path :link/fiddle :link/formula :link/tx-fn])
                                       #_(s/multi-spec fiddle-link :link/class))))
(s/def :fiddle/markdown string?)
(s/def :fiddle/renderer string?)
(s/def :fiddle/css string?)
(s/def :fiddle/cljs-ns string?)
(s/def :fiddle/hydrate-result-as-fiddle string?)

(s/def :link/class (s/coll-of keyword?))
;(s/def :link/fiddle (s/keys))
(s/def :link/path string?)
(s/def :link/formula string?)
(s/def :link/tx-fn string?)

(s/def :hyperfiddle/owners (s/coll-of uuid?))

(defmethod fiddle-type :blank [_] (s/keys))
(defmethod fiddle-type :query [_] (s/keys :opt [:fiddle/query]))
(defmethod fiddle-type :entity [_] (s/keys :opt [:fiddle/pull :fiddle/pull-database]))
(defmethod fiddle-type nil [_] (s/keys))

(declare fiddle-defaults)
(declare apply-defaults)

(defn infer-query-formula [query]
  (unwrap
    #(timbre/warn %)
    (->> (contrib.reader/memoized-read-string+ query)
         (cats/=<< #(try-either (datascript.parser/parse-query %)))
         (cats/fmap (fn [{:keys [qin]}]
                      (if (->> qin
                               (remove #(and (instance? datascript.parser.BindScalar %)
                                             (instance? datascript.parser.SrcVar (:variable %))))
                               seq)
                        ; todo (juxt first second)
                        "identity"
                        "(constantly nil)"))))))

(defn read-a "Use :qin to infer the src if the link/a eschews it."
  [s qin]
  (let [[x :as xs] (->> (contrib.reader/memoized-read-edn-string+ (str "[" s "]"))
                        (unwrap #(timbre/error %)))]
    (condp = (count xs)
      2 xs
      1 ['$ x]                                              ; TODO: Use :qin to choose the right color
      ; 0 is invalid
      0 nil)))

(defn read-path [s]
  (->> (contrib.reader/memoized-read-edn-string+ (str "[" s "]"))
       (unwrap #(timbre/error %))                           ; too late to report anything to the dev
       last                                                 ; Adapt legacy to attribute
       ))

(def link-defaults
  {:link/formula (fn [link]
                   (cond

                     (some #{:hf/new :hf/affix} (:link/class link)) "(constantly (hyperfiddle.api/tempid! ctx))"

                     (:link/fiddle link)                    ; If there is a fiddle-target, infer the expected :in shape
                     (case (get-in link [:link/fiddle :fiddle/type] ((:fiddle/type fiddle-defaults) (:link/fiddle link)))
                       :query (infer-query-formula (get-in link [:link/fiddle :fiddle/query] ((:fiddle/query fiddle-defaults) (:link/fiddle link))))
                       :entity "identity"

                       ; this is the case where the v is inferred but we don't actually want it, like a toplevel iframe for a FindScalar.
                       :blank nil)

                     ; TODO: What if :txfn is combined with :target-fiddle :blank, or :target-fiddle :FindTuple?
                     :else-txfn "identity"))

   :link/tx-fn (fn [schemas qin link]
                 ; Auto parent-child management for eav ref contexts
                 (let [[src-db a] (read-a (contrib.string/blank->nil (:link/path link)) qin)
                       ; Consider: ref, fiddle-ident, scalar (e.g. :streaker/date, :fiddle/ident, :db/ident)
                       schema (get schemas (str src-db))
                       is-identity (and a (contrib.datomic/unique? schema a :db.unique/identity))
                       is-ref (and a (contrib.datomic/ref? schema a))]
                   (condp some (:link/class link)
                     #{:hf/new :hf/affix} (cond
                                            is-identity ":zero" ; hack to draw as popover
                                            is-ref ":db/add"
                                            :else ":zero")  ; nil a, or qfind-level

                     #{:hf/remove :hf/detach} (cond
                                                is-identity ":db.fn/retractEntity"
                                                is-ref ":db/retract"
                                                :else ":db.fn/retractEntity") ; legacy compat, remove
                     nil)))})

(def fiddle-defaults
  {:fiddle/markdown (fn [fiddle] (str/fmt "### %s" (some-> fiddle :fiddle/ident str)))
   :fiddle/pull (constantly "[:db/id *]")
   :fiddle/pull-database (constantly "$")
   :fiddle/query (constantly (template/load-resource "fiddle-query-default.edn"))
   :fiddle/renderer (fn [fiddle]
                      #?(:cljs (-> hyperfiddle.ui/fiddle meta :expr-str)
                         :clj  nil))
   :fiddle/type (constantly :blank)})                       ; default is :query from new-fiddle; but from links panel, it's :entity

(defn auto-link [schemas qin link]
  (-> link
      (update-existing :link/fiddle apply-defaults)
      (update :link/formula or-str ((:link/formula link-defaults) link))
      (update :link/tx-fn or-str ((:link/tx-fn link-defaults) schemas qin link))))

(defn apply-defaults "Fiddle-level defaults but not links.
  Links need the qfind, but qfind needs the fiddle defaults.
  Don't depend on ctx, this function runs in many situations including in datalog"
  [fiddle]
  (as-> fiddle fiddle
        (update fiddle :fiddle/type #(or % ((:fiddle/type fiddle-defaults) fiddle)))
        (cond-> fiddle
                (= :query (:fiddle/type fiddle)) (update :fiddle/query or-str ((:fiddle/query fiddle-defaults) fiddle))
                (= :entity (:fiddle/type fiddle)) (-> (update :fiddle/pull or-str ((:fiddle/pull fiddle-defaults) fiddle))
                                                      (update :fiddle/pull-database or-str ((:fiddle/pull-database fiddle-defaults) fiddle))))
        (update fiddle :fiddle/markdown or-str ((:fiddle/markdown fiddle-defaults) fiddle))
        (update fiddle :fiddle/renderer or-str ((:fiddle/renderer fiddle-defaults) fiddle))))

(defn apply-fiddle-links-defaults "Link defaults require a parsed qfind, so has to be done separately later."
  [fiddle schemas qparsed]
  (update fiddle :fiddle/links (partial map (partial auto-link schemas (:qin qparsed)))))

(def browser-pull                                           ; synchronized with http://hyperfiddle.hyperfiddle.net/:hyperfiddle!ide/
  [:db/id
   :fiddle/css
   :fiddle/ident
   :fiddle/uuid
   {:fiddle/links [:db/id
                   :link/class
                   {:link/fiddle [:db/id
                                  :fiddle/ident             ; routing
                                  :fiddle/query             ; validation
                                  :fiddle/type              ; validation
                                  ]}
                   :link/formula
                   :link/ident                              ; legacy
                   :link/path
                   :link/tx-fn]}
   :fiddle/markdown
   :fiddle/pull
   :fiddle/pull-database
   :fiddle/query
   :fiddle/cljs-ns
   :fiddle/renderer
   :fiddle/type
   :fiddle/hydrate-result-as-fiddle
   '*                                                       ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer
   ])


(defn parse-fiddle-query [{:keys [fiddle/type fiddle/query fiddle/pull fiddle/pull-database]}]
  (->> (case type
         :blank nil
         :entity (->> (contrib.reader/memoized-read-edn-string+ pull)
                      (fmap (fn [pull]
                              (let [source (symbol pull-database)
                                    fake-q `[:find (~'pull ~source ~'?e ~pull) . :where [~'?e]]]
                                (datascript.parser/parse-query fake-q))))
                      (unwrap (constantly nil)))
         :query (->> (contrib.reader/memoized-read-edn-string+ query)
                     (=<< #(try-either
                             (datascript.parser/parse-query %)))
                     (unwrap (constantly nil))))))
