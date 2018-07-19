(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive :as r]
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.reagent :refer [fragment]]
    [hypercrud.browser.link :as link :refer [rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.ui.form :as form]
    [hyperfiddle.ui.select :refer [select]]))


(defn hyper-select-head [props ctx]
  (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)]
    (fragment (if (and (= :xray display-mode)
                       (not (:link/dependent? @(r/track rel->link :options ctx))))
                ; Float right
                [select props ctx])
              [form/label props ctx]
              [anchors (:hypercrud.browser/path ctx) props ctx link/options-processor]
              [iframes (:hypercrud.browser/path ctx) props ctx link/options-processor])))

(defn hyper-label [props ctx]
  (fragment [form/label props ctx]
            [anchors (:hypercrud.browser/path ctx) props ctx]
            [iframes (:hypercrud.browser/path ctx) props ctx]))
