(ns hypercrud.ui.code-editor
  (:require [reagent.core :as reagent]
            [reagent.impl.component :as r-comp]))


(defn sync-changed-props! [ref props]
  (doseq [[prop val] props]
    (if-not (= val (.getOption ref (name prop)))
      (.setOption ref (name prop) val))))

(def code-editor*

  ;; all usages of value (from react lifecycle) need to be (str value), because
  ;; codemirror throws NPE if value is nil

  (reagent/create-class
    {:reagent-render
     (fn [value change! props]
       [:div.code-editor-wrapper
        [:textarea {:default-value (str value) :auto-complete "off" :class "text"}]])

     :component-did-mount
     (fn [this]
       (let [[_ value change! props] (r-comp/get-argv this) ;[value change! props] (reagent/props this)
             div (.querySelector (reagent/dom-node this) "textarea")
             ref (.fromTextArea js/CodeMirror div
                                (clj->js (merge {:mode "clojure"
                                                 :lineNumbers true
                                                 :matchBrackets true
                                                 :autoCloseBrackets true
                                                 :viewportMargin js/Infinity}
                                                props)))]
         (aset this "codeMirrorRef" ref)
         (.on ref "blur" (fn [_ e]
                           (let [[_ value change! props] (r-comp/get-argv this)
                                 value' (.getValue ref)]
                             (if-not (= value value')
                               (change! value')))))))

     :component-will-unmount
     (fn [this] (.toTextArea (aget this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ value change! props] (r-comp/get-argv this)
             ref (aget this "codeMirrorRef")]
         (sync-changed-props! ref (assoc props :value (str value)))))}))
