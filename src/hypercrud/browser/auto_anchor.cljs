(ns hypercrud.browser.auto-anchor
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-link :as auto-link]))


(defn auto-anchor [anchor]
  (-> anchor
      (update :anchor/tx-fn (fn [txfn-str]
                              (if (empty? txfn-str) (auto-txfn anchor) txfn-str)))
      (update :anchor/formula (fn [fxfn-str]
                                (if (empty? fxfn-str) (auto-formula anchor) fxfn-str)))))

(defn merge-anchors [sys-anchors link-anchors]
  (->> (reduce (fn [grouped-link-anchors sys-anchor]
                 (update grouped-link-anchors
                         (:anchor/ident sys-anchor)
                         (fn [maybe-link-anchors]
                           (if maybe-link-anchors
                             (map (partial merge sys-anchor) maybe-link-anchors)
                             [sys-anchor]))))
               (group-by :anchor/ident link-anchors)
               sys-anchors)
       vals
       flatten
       doall))

(defn auto-anchors [link result param-ctx & [{:keys [ignore-user-links]}]]
  (let [sys-anchors (auto-link/system-anchors link result param-ctx)]
    (->> (if ignore-user-links
           sys-anchors
           (merge-anchors sys-anchors (:link/anchor link)))
         (map auto-anchor))))
