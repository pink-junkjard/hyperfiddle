(ns hypercrud.browser.auto-link
  (:require [cats.core :refer [mlet return]]
            [contrib.ct :refer [unwrap]]
            [contrib.data :refer [map-values]]
            [contrib.string :refer [blank->nil memoized-safe-read-edn-string]]
            [contrib.try$ :refer [try-either]]
            [contrib.reactive :as r]
            [datascript.parser]
            [hypercrud.browser.fiddle :as fiddle]
            [hypercrud.browser.system-link :as system-link]
            [taoensso.timbre :as timbre]))


(defn auto-formula [{:keys [:link/fiddle] :as link}]
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
    :blank nil
    nil))

(defn auto-link [ctx link]
  (let [auto-fn (fn [link attr auto-f]
                  (let [v (get link attr)]
                    (if (or (not v) (and (string? v) (empty? v)))
                      (assoc link attr (auto-f link))
                      link)))]
    (-> link
        (update :link/fiddle #(fiddle/data-defaults (into {} %)))
        (auto-fn :link/formula auto-formula))))

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
        sys-links (system-link/system-links @fiddle @(:hypercrud.browser/field ctx) @(:hypercrud.browser/schemas ctx))
        links (->> (merge-links sys-links @(r/cursor fiddle [:fiddle/links]))
                   (map (partial auto-link ctx)))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
