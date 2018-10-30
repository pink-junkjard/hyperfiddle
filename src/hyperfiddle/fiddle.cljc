(ns hyperfiddle.fiddle
  (:require
    [cats.core :as cats]
    [contrib.ct :refer [unwrap]]
    [contrib.reader :as reader]
    [contrib.string :refer [or-str]]
    [contrib.template :as template]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [datascript.parser]
    #_[hyperfiddle.ui]
    [taoensso.timbre :as timbre]))


(declare fiddle-defaults)
(declare apply-defaults)

(defn infer-query-formula [query]
  (unwrap
    #(timbre/warn %)
    (->> (reader/memoized-read-string+ query)
         (cats/=<< #(try-either (datascript.parser/parse-query %)))
         (cats/fmap (fn [{:keys [qin]}]
                      (if (->> qin
                               (remove #(and (instance? datascript.parser.BindScalar %)
                                             (instance? datascript.parser.SrcVar (:variable %))))
                               seq)
                        "identity"
                        "(constantly nil)"))))))

(def link-defaults
  {:link/formula (fn [link]
                   (condp contains? (:link/rel link)
                     #{:hf/new} "(constantly (hyperfiddle.api/tempid-detached ctx))"
                     #{:hf/affix} "(partial hyperfiddle.api/tempid-child ctx)"
                     #{:hf/rel :hf/self :hf/iframe}
                     (case (get-in link [:link/fiddle :fiddle/type] ((:fiddle/type fiddle-defaults) (:link/fiddle link)))
                       :query (infer-query-formula (get-in link [:link/fiddle :fiddle/query] ((:fiddle/query fiddle-defaults) (:link/fiddle link))))
                       :entity "identity"
                       :blank nil)
                     nil))
   :link/tx-fn (fn [link]
                 (case (:link/rel link)
                   :hf/new "(constantly {:tx []})"          ; hack to draw as popover
                   :hf/remove (template/load-resource "auto-txfn/remove.edn")
                   :hf/affix (template/load-resource "auto-txfn/affix.edn")
                   :hf/detach (template/load-resource "auto-txfn/detach.edn")
                   :hf/self nil
                   :hf/iframe nil
                   :hf/rel nil
                   nil))})

(def fiddle-defaults
  {:fiddle/markdown (fn [fiddle] (str/fmt "### %s" (some-> fiddle :fiddle/ident str)))
   :fiddle/pull (constantly "[:db/id *]")
   :fiddle/pull-database (constantly "$")
   :fiddle/query (constantly "[:find (pull ?e [:db/id *]) :where\n [?e :db/ident :db/add]]")
   :fiddle/renderer (fn [fiddle]
                      #?(:cljs (-> hyperfiddle.ui/fiddle meta :expr-str)
                         :clj  nil))
   :fiddle/type (constantly :blank)})

(defn auto-link [link]
  (let [link (cond-> link
               (contains? #{:hf/rel :hf/self :hf/new :hf/iframe} (:link/rel link)) (update :link/fiddle apply-defaults))]
    (-> link
        (update :link/formula or-str ((:link/formula link-defaults) link))
        (update :link/tx-fn or-str ((:link/tx-fn link-defaults) link)))))

(defn apply-defaults [fiddle]
  (-> fiddle
      (update :fiddle/links (partial map auto-link))
      (update :fiddle/type #(or % ((:fiddle/type fiddle-defaults) fiddle)))
      (cond->
        (= :query (:fiddle/type fiddle)) (update :fiddle/query or-str ((:fiddle/query fiddle-defaults) fiddle))
        (= :entity (:fiddle/type fiddle)) (-> (update :fiddle/pull or-str ((:fiddle/pull fiddle-defaults) fiddle))
                                              (update :fiddle/pull-database or-str ((:fiddle/pull-database fiddle-defaults) fiddle))))
      (update :fiddle/markdown or-str ((:fiddle/markdown fiddle-defaults) fiddle))
      (update :fiddle/renderer or-str ((:fiddle/renderer fiddle-defaults) fiddle))))
