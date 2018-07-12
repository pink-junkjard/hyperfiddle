(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.reagent :refer [fragment from-react-context]]
    [hypercrud.browser.link :as link :refer [rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.ui.form :as form]
    [hyperfiddle.ui.select :refer [select]]))


(def hyper-select-head
  (from-react-context
    (fn [{:keys [ctx props]} field]
      (let [display-mode (-> @(:hypercrud.ui/display-mode ctx) name keyword)]
        (fragment (if (and (= :xray display-mode)
                           (not (:link/dependent? (rel->link :options ctx))))
                    ; Float right
                    [select nil])
                  (when field [form/label field])
                  [anchors (:hypercrud.browser/path ctx) link/options-processor]
                  [iframes (:hypercrud.browser/path ctx) link/options-processor])))))

(def hyper-label
  (from-react-context
    (fn [{:keys [ctx props]} field]
      (fragment (when field [form/label field])
                [anchors (:hypercrud.browser/path ctx)]
                [iframes (:hypercrud.browser/path ctx)]))))
