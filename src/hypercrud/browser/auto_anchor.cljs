(ns hypercrud.browser.auto-anchor
  (:require [hypercrud.browser.auto-anchor-formula :refer [auto-formula]]
            [hypercrud.browser.auto-anchor-txfn :refer [auto-txfn]]
            [hypercrud.browser.auto-anchor-branch :refer [auto-branch]]))


(defn auto-anchor [anchor]
  (let [anchor (-> anchor
                   (update :anchor/tx-fn (fn [txfn-str]
                                           (if (empty? txfn-str) (auto-txfn anchor) txfn-str)))
                   (update :anchor/formula (fn [fxfn-str]
                                             (if (empty? fxfn-str) (auto-formula anchor) fxfn-str))))]
    ; do i need to branch?
    (if (and (:anchor/tx-fn anchor) (:anchor/link anchor))
      (update anchor :anchor/branch (fn [s]
                                      (if (empty? s) (auto-branch anchor) s)))
      anchor)))

(defn auto-anchors [anchors]
  (map auto-anchor anchors))

(defn merge-anchors [sys-anchors link-anchors]
  ; Merge the link-anchors into the sys-anchors such that matching anchors properly override.
  ; anchor uniqueness is determined by [repeat entity attr ident]. Nil ident means
  ; don't match anything. For example [nil nil nil nil] can just mean i have a lot
  ; of top level links that i didn't bother to name yet.
  (let [f (fn [e]
            [(or (-> e :anchor/repeating?) false)           ; nil matches false
             (-> e :anchor/find-element :db/id)             ; nil is okay here
             (-> e :anchor/attribute :db/id)                ; nil is okay here
             (or (-> e :anchor/ident) (:db/id e)) #_"if no ident, it's unique"])
        collated (merge-with concat (group-by f sys-anchors) (group-by f link-anchors))
        merged (map #(apply merge %) (vals collated)) #_(apply map merge (vals collated))]
    merged))
