(ns hypercrud.browser.auto-link
  (:require [cats.core :refer [mlet return]]
            [clojure.string :as string]
            [contrib.ct :refer [unwrap]]
            [contrib.data :refer [map-values]]
            [contrib.string :refer [blank->nil memoized-safe-read-edn-string]]
            [contrib.template :as template]
            [contrib.try$ :refer [try-either]]
            [contrib.reactive :as r]
            [datascript.parser]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.field :as field]
            [hypercrud.browser.fiddle :as fiddle]
            [hypercrud.browser.system-fiddle :as system-fiddle]
            [hypercrud.browser.system-link :as system-link]
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
        dbname (str (::field/source-symbol field))
        is-root (::field/source-symbol field)]

    (-> (case rel
          :hf/edit {:link/fiddle (system-fiddle/fiddle-system-edit dbname) ; manufacture pull from parent pull
                    :link/formula (auto-formula link)}
          :hf/new {:link/fiddle (system-fiddle/fiddle-system-edit dbname)
                   :link/formula (if is-root
                                   "(constantly (hyperfiddle.api/tempid-detached ctx))"
                                   "(partial hyperfiddle.api/tempid-child ctx)")
                   :link/render-inline? true
                   :link/tx-fn parent-child-txfn
                   :link/managed? true}
          :hf/remove {:link/fiddle system-fiddle/fiddle-blank-system-remove ; Future: remove this
                      :link/formula (auto-formula link)
                      :link/render-inline? true
                      :link/tx-fn retract-formula
                      :link/managed? true}
          :hf/iframe {:link/formula (auto-formula link)}

          ; Don't touch the link for rels we don't understand
          nil)

        (->> (merge-with #(or (blank->nil %1) %2) link))    ; Add back user overrides

        ; Always apply fiddle defaults (even for link rels we don't understand)
        ; (perhaps a mistake though and should be per rel, also this is a weird place to do it)
        (update :link/fiddle #(fiddle/data-defaults (into {} %)))
        )))

(defn merge-links [sys-links links]
  (->> (reduce (fn [grouped-links sys-link]
                 (update-in grouped-links
                            [(:link/rel sys-link) (blank->nil (:link/path sys-link))]
                            (fn [maybe-links]
                              (if maybe-links
                                (map (partial merge sys-link) maybe-links)
                                [sys-link]))))
               (->> links
                    (map #(into {} %))
                    (group-by #(or (:link/rel %) (:db/id %)))
                    (map-values #(group-by (comp blank->nil :link/path) %)))
               sys-links)
       vals
       (map vals)
       flatten
       doall))

; todo tighter reactivity
(defn auto-links [ctx]
  (let [fiddle (:hypercrud.browser/fiddle ctx)
        ; Ensure that any specific patterns we need are present.
        sys-links (system-link/system-links @fiddle @(:hypercrud.browser/field ctx) @(:hypercrud.browser/schemas ctx))
        links (->> (merge-links sys-links @(r/cursor fiddle [:fiddle/links]))
                   (map (partial auto-link ctx)))

        ; Take the links we got. Shadow in the defaults based on rel.
        links (map (partial auto-link ctx) links)]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
