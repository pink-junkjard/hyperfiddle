(ns hypercrud.ui.code-editor
  (:require [reagent.core :as reagent]))


(defn did-mount! [value change!]
  (fn [this]
    (let [div (.querySelector (reagent/dom-node this) "textarea")
          ref (.fromTextArea js/CodeMirror div
                             #js {:mode "clojure"
                                  :lineNumbers true
                                  :matchBrackets true
                                  :autoCloseBrackets true
                                  :viewportMargin js/Infinity})]
      (aset this "codeMirrorRef" ref)
      (.on ref "blur" (let [last-val (atom value)]
                        (fn [e]
                          (let [old @last-val
                                new (.getValue e)]
                            (reset! last-val new)
                            (change! [old] [new]))))))))


(defn code-editor* [value change!]
  (reagent/create-class
    {:render (fn [this]
               [:div.code-editor
                [:textarea {:default-value value :auto-complete "off" :class "text"}]])
     :component-did-mount (did-mount! value change!)
     :component-will-unmount (fn [this] (.toTextArea (aget this "codeMirrorRef")))

     ;:component-did-update #() ; use did-update to punch through the new change! handler,
     ; so we can use tx/update-entity-attr as we would like
     }))
