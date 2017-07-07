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
  (let [f (fn [anchor]
            (let [ident (:anchor/ident anchor)]
              (if (or (= ident :sys) (= ident :remove) (= ident :options))
                (js/Math.random)
                ident)))
        collated (merge-with concat (group-by f sys-anchors) (group-by f link-anchors))
        merged (map #(apply merge %) (vals collated)) #_(apply map merge (vals collated))]
    merged))
