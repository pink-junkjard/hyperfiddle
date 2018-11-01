(ns contrib.ui.codemirror
  (:require
    [contrib.data :refer [orp]]
    [cuerdas.core :as str]
    [goog.object :as object]
    [reagent.core :as reagent]))


(defn camel-keys "Bad anti-abstraction to undo Reagent's key obfuscating" [m]
  (reduce-kv (fn [acc k v]
               (assoc acc (keyword (str/camel (name k))) v))
             {} m))

(defn sync-changed-props! [ref props]
  (doseq [[prop val] props
          :let [option (str/camel (name prop))]             ; casing hacks see https://github.com/hyperfiddle/hyperfiddle/issues/497
          :when (not= val (.getOption ref option))]
    (.setOption ref option val)))

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
     (fn [props]
       [contrib.ui/textarea (assoc (select-keys props [:id :class :default-value :on-change :value]) ; #318 Warning: React does not recognize the `lineNumbers` prop on a DOM element
                              :auto-complete "off")])

     :component-did-mount
     (fn [this]
       ; Codemirror will default to the first mode loaded in preamble
       (let [[_ props] (reagent/argv this)
             ref (js/CodeMirror.fromTextArea (reagent/dom-node this)
                                             (-> (dissoc props :on-change)
                                                 camel-keys ; casing hacks see https://github.com/hyperfiddle/hyperfiddle/issues/497
                                                 clj->js))]

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
         (.on ref "change" (fn [_ e]
                             (let [[_ {:keys [on-change] :as props}] (reagent/argv this)]
                               (when on-change
                                 (let [value (orp seq (.getValue ref))]
                                   (when (not= value (:value props))
                                     (if (= value (:default-value props))
                                       (on-change nil)
                                       (on-change value))))))))

         (.on ref "blur" (fn [_ e]
                           (when (and (some? (:default-value props)) (= (.getValue ref) ""))
                             (.setValue ref (:default-value props)))))))

     :component-will-unmount
     (fn [this]
       (.toTextArea (object/get this "codeMirrorRef")))

     :component-did-update
     (fn [this]
       (let [[_ props] (reagent/argv this)
             ref (object/get this "codeMirrorRef")
             new-value (orp some? (:value props) (:default-value props) "")
             current-value (.getValue ref)]
         ; internal CM value state != ctor props
         (when (and (not= current-value new-value)
                    (not (and (= "" current-value) (= (:default-value props) new-value)))
                    (not (and (= "" new-value) (= (:default-value props) current-value))))
           (.setValue ref new-value))
         (sync-changed-props! ref (dissoc props :default-value :value :on-change))))}))
