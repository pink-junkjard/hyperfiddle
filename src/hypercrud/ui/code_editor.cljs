(ns hypercrud.ui.code-editor
  (:require [reagent.core :as reagent]))


(defn did-mount! [value change!]
  (fn [this]
    (let [pane (.fromTextArea
                 js/CodeMirror
                 (reagent/dom-node this)
                 #js {:mode "clojure"
                      :lineNumbers true
                      :matchBrackets true
                      :autoCloseBrackets true
                      :viewportMargin js/Infinity})]
      (.on pane "change" #(change! [:db/retract value]
                                   [:db/add (.getValue %)])))))

(defn code-editor* [value change!]
  (reagent/create-class
    {:render (fn []
               [:textarea {:default-value value :auto-complete "off" :class "text"}])
     :component-did-mount (did-mount! value change!)}))
