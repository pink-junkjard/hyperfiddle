(ns contrib.ui.codemirror
  (:require
    [cuerdas.core :as str]
    [goog.object :as object]
    [reagent.core :as reagent]))


(defn camel-keys "Bad anti-abstraction to undo Reagent's key obfuscating" [m]
  (reduce-kv (fn [acc k v]
               (assoc acc (keyword (str/camel (name k))) v))
             {} m))

(defn sync-changed-props! [ref props]
  (doseq [[prop val] props]
    (if-not (= val (.getOption ref (name prop)))
      (.setOption ref (name prop) val))))

(defn ensure-mode [ref new-mode]
  (js/parinferCodeMirror.setMode ref new-mode)
  #_(js/parinferCodeMirror.setOptions ref #js {"locus" (= new-mode "indent")})
  (doto (-> ref .getWrapperElement .-classList)
    (.remove "paren")
    (.remove "indent")
    (.add new-mode)))

(def -codemirror

  ; all usages of value (from react lifecycle) need to be (str value), because
  ; codemirror throws NPE if value is nil

  (reagent/create-class
    {:reagent-render
     (fn [value change! props]
       [:textarea (merge (select-keys props [:id :class])   ; #318 Warning: React does not recognize the `lineNumbers` prop on a DOM element
                         {:default-value (str value)
                          :auto-complete "off"})])

     :component-did-mount
     (fn [this]
       ; Codemirror will default to the first mode loaded in preamble
       (let [[_ value change! props] (reagent/argv this)
             ref (js/CodeMirror.fromTextArea (reagent/dom-node this) (clj->js props))]

         (when (and (:parinfer props) (= "clojure" (:mode props)))
           ; `mode` is 'paren', 'indent', or 'smart'
           (js/parinferCodeMirror.init ref "paren" #js {"locus" true})
           (ensure-mode ref "paren")                        ; sets up css
           (.addKeyMap ref #js {"Ctrl-1" #(let [cur-mode (goog.object/getValueByKeys ref "__parinfer__" "mode")]
                                            (ensure-mode ref (case cur-mode "paren" "indent" "paren")))}))

         ; Props are a shitshow. Remark is stringly, and codemirror wants js types.
         ; set `lineNumber=` to disable line numbers (empty string is falsey).
         ; Also, reagent/as-element keyword-cases all the props into keywords, so
         ; they must be camelized before we get here.

         (object/set this "codeMirrorRef" ref)
         (.on ref "blur" (fn [_ e]
                           (let [[_ value change! props] (reagent/argv this)
                                 value' (.getValue ref)]
                             (if-not (= value value')
                               (if change! (change! value'))))
                           nil))))

     :component-will-unmount
     (fn [this]
       (.toTextArea (object/get this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ value change! props] (reagent/argv this)
             ref (object/get this "codeMirrorRef")]
         (sync-changed-props! ref (assoc props :value (str value)))))}))
