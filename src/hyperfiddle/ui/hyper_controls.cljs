(ns hyperfiddle.ui.hyper-controls
  (:require
    [contrib.reactive-debug :refer [track-cmp]]
    [contrib.reagent :refer [fragment from-react-context]]
    [hypercrud.browser.link :as link :refer [links-here rel->link]]
    [hypercrud.ui.control.link-controls :refer [anchors iframes]]
    [hyperfiddle.ui.controls :as controls]
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

(def hyper-select
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [i (:fe-pos ctx)
            a (:hypercrud.browser/attribute ctx)]
        (fragment [anchors :body i a link/options-processor] ; Order sensitive, here be floats
                  [select value]
                  [iframes :body i a link/options-processor])))))

(def hyper-label
  (from-react-context
    (fn [{:keys [ctx props]} field]
      (fragment (when field [form/label field])
                [anchors (:hypercrud.browser/path ctx)]
                [iframes (:hypercrud.browser/path ctx)]))))
