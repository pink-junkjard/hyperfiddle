(ns hypercrud.ui.code-editor
  (:require [reagent.core :as reagent]
            [reagent.impl.component :as r-comp]))


(def code-editor*
  (reagent/create-class
    {:reagent-render
     (fn [value change!]
       [:div.value
        [:textarea {:default-value value :auto-complete "off" :class "text"}]])

     :component-did-mount
     (fn [this]
       (let [[_ value change!] (r-comp/get-argv this)       ;[value change!] (reagent/props this)
             div (.querySelector (reagent/dom-node this) "textarea")
             ref (.fromTextArea js/CodeMirror div
                                #js {:mode "clojure"
                                     :lineNumbers true
                                     :matchBrackets true
                                     :autoCloseBrackets true
                                     :viewportMargin js/Infinity})]
         (aset this "codeMirrorRef" ref)
         (.on ref "blur" #(change! (.getValue %)))))


     :component-will-unmount
     (fn [this] (.toTextArea (aget this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ value change!] (r-comp/get-argv this)]      ;[value change!] (reagent/props this)
         (.setValue (aget this "codeMirrorRef") value)))}))
