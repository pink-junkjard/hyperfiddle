(ns hypercrud.ui.code-editor
  (:require [reagent.core :as reagent]))


(defn did-mount! [change!]
  (fn [this]
    (let [div (.querySelector (reagent/dom-node this) "textarea")
          ref (.fromTextArea js/CodeMirror div
                             #js {:mode "clojure"
                                  :lineNumbers true
                                  :matchBrackets true
                                  :autoCloseBrackets true
                                  :viewportMargin js/Infinity})]
      (aset this "codeMirrorRef" ref)
      (.on ref "blur" #(change! (.getValue %))))))


(defn code-editor* [value change!]
  (reagent/create-class
    {:render (fn [this]
               [:div.value
                [:textarea {:default-value value :auto-complete "off" :class "text"}]])
     :component-did-mount (did-mount! change!)
     :component-will-unmount (fn [this] (.toTextArea (aget this "codeMirrorRef")))

     :component-did-update (fn [this props]
                             (let [[_ value change!] (aget this "props" "argv")] ; wtf reagent
                               (.setValue (aget this "codeMirrorRef") value)))}))
