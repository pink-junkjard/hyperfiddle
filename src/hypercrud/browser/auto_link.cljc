(ns hypercrud.browser.auto-link
  (:require [cats.core :refer [mlet return]]
            [clojure.string :as string]
            [contrib.ct :refer [unwrap]]
            [contrib.data :refer [map-values update-existing]]
            [contrib.string :refer [blank->nil memoized-safe-read-edn-string or-str]]
            [contrib.template :as template]
            [contrib.try$ :refer [try-either]]
            [contrib.reactive :as r]
            [datascript.parser]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.fiddle :as fiddle]
            [taoensso.timbre :as timbre]
            [hypercrud.browser.link :as link]))


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

(def parent-child-txfn (-> (template/load-resource "auto-txfn/mt-fet-at.edn") string/trim))
(def new-child-formula "(partial hyperfiddle.api/tempid-child ctx)")
(def new-formula "(constantly (hyperfiddle.api/tempid-detached ctx))")
(def retract-formula
  "(fn [ctx multi-color-tx modal-route]
  {:tx {(hypercrud.browser.context/uri ctx) [[:db.fn/retractEntity @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])]]}})")

(defn auto-link [ctx {:keys [:link/rel :link/fiddle] :as link}]
  (let [path (-> link :link/path link/read-path)
        field @(:hypercrud.browser/field (context/refocus ctx path)) ; can't focus without data
        is-root (::field/source-symbol field)]
    ; Don't crash if we don't understand the rel
    (let [a (case rel
              :hf/new {:link/formula (if is-root new-formula new-child-formula)
                       :link/tx-fn parent-child-txfn}
              :hf/remove {:link/tx-fn retract-formula}
              :hf/edit {}                                   ; We know this is an anchor, otherwise pull deeper instead
              :hf/iframe {}                                 ; iframe is always a query, otherwise pull deeper instead. Today this defaults in the add-fiddle txfn
              nil)

          ; apply userland tweaks
          b (merge-with #(or (blank->nil %1) %2) a link)

          ; Shadow the fiddle
          c (condp contains? rel
              #{:hf/edit :hf/new :hf/iframe} (update-existing b :link/fiddle #(fiddle/data-defaults (into {} %))) ; default form and title
              b)

          ; Formula inference needs known query value
          d (let [{{query :fiddle/query :as fiddle} :link/fiddle} c]
              (condp contains? rel
                #{:hf/iframe} (update c :link/formula or-str (cond
                                                               query (infer-query-formula query)
                                                               fiddle "(constantly nil)"
                                                               :else nil))
                #{:hf/edit} (update c :link/formula or-str (cond
                                                             query (infer-query-formula query)
                                                             fiddle "identity"
                                                             :else nil))
                c))]
      d)))

; todo tighter reactivity
(defn auto-links [ctx]
  (let [links (map (partial auto-link ctx) @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/links]))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
