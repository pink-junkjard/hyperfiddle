(ns contrib.ui.codemirror
  (:require
    [cuerdas.core :as str]
    [reagent.core :as reagent]))


(defn camel-keys "Useless anti-abstraction to undo Reagent's key obfuscating" [m]
  (reduce-kv (fn [acc k v]
               (assoc acc (keyword (str/camel (name k))) v))
             {} m))

(defn sync-changed-props! [ref props]
  (doseq [[prop val] props]
    (if-not (= val (.getOption ref (name prop)))
      (.setOption ref (name prop) val))))

(def -codemirror

  ;; all usages of value (from react lifecycle) need to be (str value), because
  ;; codemirror throws NPE if value is nil

  (reagent/create-class
    {:reagent-render
     (fn [value change! props]
       [:textarea (merge (select-keys props [:id :class])   ; #318 Warning: React does not recognize the `lineNumbers` prop on a DOM element
                         {:default-value (str value)
                          :auto-complete "off"})])

     :component-did-mount
     (fn [this]
       (let [[_ value change! props] (reagent/argv this)    ;[value change! props] (reagent/props this)
             ref (js/CodeMirror.fromTextArea (reagent/dom-node this) (clj->js props))]
         #_(println (pr-str (clj->js props)))
         ; Props are a shitshow. Remark is stringly, and codemirror wants js types.
         ; set `lineNumber=` to disable line numbers (empty string is falsey).
         ; Also, reagent/as-element keyword-cases all the props into keywords, so
         ; they must be camelized before we get here.

         (aset this "codeMirrorRef" ref)
         (.on ref "blur" (fn [_ e]
                           (let [[_ value change! props] (reagent/argv this)
                                 value' (.getValue ref)]
                             (if-not (= value value')
                               (if change! (change! value'))))
                           nil))))

     :component-will-unmount
     (fn [this]
       (.toTextArea (aget this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ value change! props] (reagent/argv this)
             ref (aget this "codeMirrorRef")]
         (sync-changed-props! ref (assoc props :value (str value)))))}))
