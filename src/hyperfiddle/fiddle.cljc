(ns hyperfiddle.fiddle
  (:require
    [cats.core :refer [mlet return]]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [update-existing]]
    [contrib.string :refer [blank->nil memoized-safe-read-edn-string or-str]]
    [contrib.template :as template]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [datascript.parser]
    #_[hyperfiddle.ui]
    [taoensso.timbre :as timbre]))


(declare data-defaults)

(defn infer-query-formula [query]
  (unwrap
    #(timbre/warn %)
    (mlet [q (memoized-safe-read-edn-string query)
           {qin :qin} (try-either (if q (datascript.parser/parse-query q)))]
          ; [{:variable {:symbol $}}{:variable {:symbol ?gender}}]
          (return
            (if (seq (drop 1 qin))                              ; Removing the rules and src is hard with the bind wrappers so yolo
              "identity"
              "(constantly nil)")))))

(def txfn-affix (-> (template/load-resource "auto-txfn/affix.edn") string/trim))
(def txfn-detach (-> (template/load-resource "auto-txfn/detach.edn") string/trim))
(def txfn-remove (-> (template/load-resource "auto-txfn/remove.edn") string/trim))
(def tempid-child "(partial hyperfiddle.api/tempid-child ctx)")
(def tempid-detached "(constantly (hyperfiddle.api/tempid-detached ctx))")

(defn auto-link [{:keys [:link/rel] :as link}]
  ; Don't crash if we don't understand the rel
  (let [a (case rel
            :hf/new {:link/formula tempid-detached :link/tx-fn "(constantly {:tx []})"} ; hack to draw as popover
            :hf/remove {:link/tx-fn txfn-remove}
            :hf/affix {:link/formula tempid-child :link/tx-fn txfn-affix}
            :hf/detach {:link/tx-fn txfn-detach}
            :hf/edit {}                                     ; We know this is an anchor, otherwise pull deeper instead
            :hf/iframe {}                                   ; iframe is always a query, otherwise pull deeper instead. Today this defaults in the add-fiddle txfn
            :hf/rel {}
            nil)

        ; apply userland tweaks
        b (merge-with #(or %2 (blank->nil %1)) a link)

        ; Shadow the fiddle
        c (condp contains? rel
            #{:hf/rel :hf/edit :hf/new :hf/iframe} (update-existing b :link/fiddle #(data-defaults (into {} %))) ; default form and title
            b)

        ; Formula inference needs known query value
        d (let [{{query :fiddle/query :as fiddle} :link/fiddle} c]
            (condp contains? rel

              #{:hf/iframe}
              (update c :link/formula or-str (cond
                                               query (infer-query-formula query)
                                               fiddle "(constantly nil)" ; why?
                                               :else nil))

              #{:hf/rel :hf/edit}
              (update c :link/formula or-str (cond
                                               query (infer-query-formula query)
                                               fiddle "identity"
                                               :else nil))
              c))]
    d))

(defn data-defaults [fiddle]
  (-> fiddle
      (update :fiddle/links (partial map auto-link))
      (cond->
        (= :query (:fiddle/type fiddle)) (update :fiddle/query or-str "[:find (pull ?e [:db/id *]) :where\n [?e :db/ident :db/add]]")
        (= :entity (:fiddle/type fiddle)) (-> (update :fiddle/pull or-str "[:db/id *]")
                                              (update :fiddle/pull-database or-str "$"))
        (nil? (:fiddle/type fiddle)) (assoc :fiddle/type :blank))))

(defn fiddle-defaults [fiddle route]
  (-> (data-defaults fiddle)
      (update :fiddle/markdown or-str (str/fmt "### %s" (some-> fiddle :fiddle/ident str)))
      (update :fiddle/renderer or-str #?(:clj nil :cljs (-> hyperfiddle.ui/fiddle meta :expr-str)))))
