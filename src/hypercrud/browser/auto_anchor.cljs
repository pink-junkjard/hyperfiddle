(ns hypercrud.browser.auto-anchor
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-link :as auto-link]))


(defn auto-anchor [anchor]
  (let [auto-fn (fn [anchor attr auto-f]
                  (let [v (get anchor attr)]
                    (if (or (not v) (and (string? v) (empty? v)))
                      (assoc anchor attr (auto-f anchor))
                      anchor)))]
    (-> anchor
        (auto-fn :anchor/tx-fn auto-txfn)
        (auto-fn :anchor/formula auto-formula))))

(defn merge-anchors [sys-anchors link-anchors]
  (->> (reduce (fn [grouped-link-anchors sys-anchor]
                 (update grouped-link-anchors
                         (:anchor/ident sys-anchor)
                         (fn [maybe-link-anchors]
                           (if maybe-link-anchors
                             (map (partial merge sys-anchor) maybe-link-anchors)
                             [sys-anchor]))))
               (group-by #(or (:anchor/ident %) (:db/id %)) link-anchors)
               sys-anchors)
       vals
       flatten
       doall))

(defn auto-anchors [link colspec query-params param-ctx & [{:keys [ignore-user-links]}]]
  (let [sys-anchors (auto-link/system-anchors link colspec query-params param-ctx)]
    (->> (if ignore-user-links
           sys-anchors
           (merge-anchors sys-anchors (:link/anchor link)))
         (map auto-anchor))))
