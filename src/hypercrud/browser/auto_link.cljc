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

(def txfn-attach (-> (template/load-resource "auto-txfn/attach.edn") string/trim))
(def txfn-detach (-> (template/load-resource "auto-txfn/detach.edn") string/trim))
(def txfn-remove (-> (template/load-resource "auto-txfn/remove.edn") string/trim))
(def tempid-child "(partial hyperfiddle.api/tempid-child ctx)")
(def tempid-detached "(constantly (hyperfiddle.api/tempid-detached ctx))")

(defn auto-link [ctx {:keys [:link/rel :link/fiddle] :as link}]
  (let [path (-> link :link/path link/read-path)
        field @(:hypercrud.browser/field (context/refocus ctx path)) ; can't focus without data
        is-root (:hypercrud.browser/parent ctx)]
    ; Don't crash if we don't understand the rel
    (let [a (case rel
              :hf/new {:link/formula (if is-root tempid-detached tempid-child)
                       :link/tx-fn (if is-root "(constantly {:tx []})" txfn-attach)}
              :hf/remove {:link/tx-fn txfn-remove}
              :hf/detach {:link/tx-fn txfn-detach}
              :hf/edit {}                                   ; We know this is an anchor, otherwise pull deeper instead
              :hf/iframe {}                                 ; iframe is always a query, otherwise pull deeper instead. Today this defaults in the add-fiddle txfn
              :hf/rel {}
              nil)

          ; apply userland tweaks
          b (merge-with #(or (blank->nil %1) %2) a link)

          ; Shadow the fiddle
          c (condp contains? rel
              #{:hf/rel :hf/edit :hf/new :hf/iframe} (update-existing b :link/fiddle #(fiddle/data-defaults (into {} %))) ; default form and title
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
      d)))

; todo tighter reactivity
(defn auto-links [ctx]
  (let [links (map (partial auto-link ctx) @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/links]))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
