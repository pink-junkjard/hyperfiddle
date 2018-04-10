(ns hypercrud.browser.auto-link
  (:require [contrib.data :as util]
            [contrib.reactive :as reactive]
            [hypercrud.browser.auto-link-formula :refer [auto-formula]]
            [hypercrud.browser.auto-link-txfn :refer [auto-txfn]]
            [hypercrud.browser.system-link :as system-link]))


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
                 (update-in grouped-links
                            [(:link/rel sys-link) (:link/path sys-link)]
                            (fn [maybe-links]
                              (if maybe-links
                                (map (partial merge sys-link) maybe-links)
                                [sys-link]))))
               (->> links
                    (map #(into {} %))
                    (group-by #(or (:link/rel %) (:db/id %)))
                    (util/map-values #(group-by :link/path %)))
               sys-links)
       vals
       (map vals)
       flatten
       doall))

; todo tighter reactivity
(defn auto-links [ctx]
  (let [fiddle (:hypercrud.browser/fiddle ctx)
        sys-links (system-link/system-links @fiddle @(:hypercrud.browser/ordered-fes ctx) @(:hypercrud.browser/schemas ctx))
        links (->> (merge-links sys-links @(reactive/cursor fiddle [:fiddle/links]))
                   (map auto-link))]
    (if (:keep-disabled-anchors? ctx)
      links
      (remove :link/disabled? links))))
