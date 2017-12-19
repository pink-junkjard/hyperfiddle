(ns hypercrud.ui.attribute.markdown-editor
  (:require [hypercrud.ui.attribute.code :refer [code]]))


(defn ^:export markdown-editor [maybe-field links props ctx]
  (let [props (assoc props :mode "markdown" :lineWrapping true)]
    [code maybe-field links props ctx]))
