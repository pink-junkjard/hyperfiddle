(ns hypercrud.browser.auto-anchor
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]))


(defn auto-anchor [anchor]
  (-> anchor
      (update :anchor/tx-fn (fn [txfn-str]
                              (if (empty? txfn-str) (auto-txfn anchor) txfn-str)))
      (update :anchor/formula (fn [fxfn-str]
                                (if (empty? fxfn-str) (auto-formula anchor) fxfn-str)))))

(defn auto-anchors [anchors]
  (map auto-anchor anchors))

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
