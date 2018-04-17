(ns hypercrud.browser.popovers
  (:require [hypercrud.browser.auto-link-formula :as auto-link-formula]
            [hypercrud.util.branch :as branch]
            [contrib.data :refer [abs-normalized]]))


; todo same implementation as branch generation
(defn popover-id [link ctx]
  ; we should run the auto-formula logic to determine an appropriate auto-id fn
  (let [child-id-str (-> [(auto-link-formula/deterministic-ident ctx) (:db/id link)]
                         hash abs-normalized - str)]
    (branch/encode-branch-child (:branch ctx) child-id-str)))

(defn branch [ctx link]
  (if (:link/managed? link)
    ; we should run the auto-formula logic to determine an appropriate auto-id fn
    (let [child-id-str (-> [(auto-link-formula/deterministic-ident ctx) (:db/id link)]
                           hash abs-normalized - str)]
      (branch/encode-branch-child (:branch ctx) child-id-str))
    (:branch ctx)))
