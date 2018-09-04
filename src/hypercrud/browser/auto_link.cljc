(ns hypercrud.browser.auto-link
  (:require [cats.core :refer [mlet return]]
            [clojure.string :as string]
            [contrib.ct :refer [unwrap]]
            [contrib.data :refer [map-values]]
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


(defn auto-formula [{:keys [:link/fiddle :link/rel :link/path]}]
  ; This is for :anchor, :iframe, :option - anything in the system.
  ; handle root case, by inspecting the target fiddle query input args
  (case (:fiddle/type fiddle)
    :query
    (unwrap
      #(timbre/warn %)
      (mlet [q (memoized-safe-read-edn-string (:fiddle/query fiddle))
             {qin :qin} (try-either (if q (datascript.parser/parse-query q)))]
        ; [{:variable {:symbol $}}{:variable {:symbol ?gender}}]
        (return
          (if (seq (drop 1 qin))                            ; Removing the rules and src is hard with the bind wrappers so yolo
            "identity"
            "(constantly nil)"))))
    :entity "identity"
    :blank "(constantly nil)"
    "(constantly nil)"))

(def parent-child-txfn (-> (template/load-resource "auto-txfn/mt-fet-at.edn") string/trim))

(def retract-formula
  "(fn [ctx multi-color-tx modal-route]
  {:tx {(hypercrud.browser.context/uri ctx) [[:db.fn/retractEntity @(contrib.reactive/cursor (:hypercrud.browser/data ctx) [:db/id])]]}})")

(defn auto-link [ctx {:keys [:link/rel] :as link}]
  (let [path (-> link :link/path link/read-path)
        field @(:hypercrud.browser/field (context/refocus ctx path)) ; can't focus without data
        is-root (::field/source-symbol field)]
    ; Don't crash if we don't understand the rel
    (let [a (case rel
              :hf/new {:link/formula (if is-root
                                       "(constantly (hyperfiddle.api/tempid-detached ctx))"
                                       "(partial hyperfiddle.api/tempid-child ctx)")
                       :link/tx-fn parent-child-txfn}
              :hf/remove {:link/tx-fn retract-formula}
              :hf/edit {}                                   ; We know this is an anchor, otherwise pull deeper instead
              :hf/iframe {}                                 ; iframe is always a query, otherwise pull deeper instead. Today this defaults in the add-fiddle txfn
              nil)
          b (merge-with #(or (blank->nil %1) %2) a link)    ; apply userland overrides
          c (condp contains? rel
              #{:hf/edit :hf/new :hf/iframe} (update b :link/fiddle #(fiddle/data-defaults (into {} %))) ; default form and title
              #{:hf/remove} b
              b)

          ; Formula inspects the fiddle query
          d (condp contains? rel
              #{:hf/edit :hf/iframe} (update c :link/formula or-str (auto-formula c))
              #{:hf/remove :hf/new} c
              c)]
      d)))

; todo tighter reactivity
(defn auto-links [ctx]
  (let [links (map (partial auto-link ctx) @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/links]))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
