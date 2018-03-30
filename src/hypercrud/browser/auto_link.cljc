(ns hypercrud.browser.auto-link
  (:require [hypercrud.browser.auto-link-formula :refer [auto-formula]]
            [hypercrud.browser.auto-link-txfn :refer [auto-txfn]]
            [hypercrud.browser.system-link :as system-link]
            [contrib.reactive :as reactive]))


(defn auto-link [link]
  (let [auto-fn (fn [link attr auto-f]
                  (let [v (get link attr)]
                    (if (or (not v) (and (string? v) (empty? v)))
                      (assoc link attr (auto-f link))
                      link)))]
    (-> link
        (auto-fn :link/tx-fn auto-txfn)
        (auto-fn :link/formula auto-formula))))

(defn merge-links [sys-links links]
  (->> (reduce (fn [grouped-links sys-link]
                 (update grouped-links
                         (:link/rel sys-link)
                         (fn [maybe-links]
                           (if maybe-links
                             (map (partial merge sys-link) maybe-links)
                             [sys-link]))))
               (->> links
                    (map #(into {} %))
                    (group-by #(or (:link/rel %) (:db/id %))))
               sys-links)
       vals
       flatten
       doall))

; todo tighter reactivity
(defn auto-links [fiddle ordered-fes schemas & [keep-disabled-anchors?]]
  (let [sys-links (system-link/system-links @fiddle @ordered-fes @schemas)
        links (->> (merge-links sys-links @(reactive/cursor fiddle [:fiddle/links]))
                   (map auto-link))]
    (if keep-disabled-anchors?
      links
      (remove :link/disabled? links))))
