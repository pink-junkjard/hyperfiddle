(ns hyperfiddle.ui.markdown-
  (:require
    [clojure.walk :refer [keywordize-keys]]
    [contrib.css :refer [classes]]
    [contrib.data :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.string :refer [memoized-safe-read-edn-string or-str]]
    [contrib.ui]
    [contrib.ui.remark :as remark]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hyperfiddle.core :refer [read-eval-with-bindings]]
    [reagent.core :as reagent]))


(declare remarkInstance)

(def markdown-
  ; remark creates react components which don't evaluate in this stack frame
  ; so dynamic scope is not helpful to communicate values to remark plugins
  ; Reagent + react-context: https://github.com/reagent-project/reagent/commit/a8ec0d219bbd507f51a4d9276c4a1dcc020245af
  (reagent/create-class
    {:display-name "hf-markdown"
     :reagent-render
     (fn [value & [?ctx]]
       (when-not (or (nil? value) (str/blank? value))
         (remark/remark-render remarkInstance value)))

     :get-child-context
     (fn []
       (this-as this
         (let [[_ value ?ctx] (reagent/argv this)]
           #js {:ctx ?ctx})))

     :child-context-types #js {:ctx js/propTypes.object}}))

(defn md-extension [f]
  (reagent/create-class
    {:context-types #js {:ctx js/propTypes.object}
     :reagent-render (fn [{:keys [content argument] :as props}]
                       (let [ctx (goog.object/get (.-context (reagent/current-component)) "ctx")
                             content (if-not (= content "undefined") content)
                             argument (if-not (= argument "undefined") argument)]
                         (f content argument (dissoc props :content :argument) ctx)))}))

(defn eval [content argument props ctx]
  (read-eval-with-bindings content ctx))

(defn browse [content argument props ctx]
  (let [kwargs (flatten (seq props))
        [_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        f (read-eval-with-bindings content)]
    (apply (:browse ctx) rel path ctx f kwargs)))

(defn anchor [content argument props ctx]
  (let [kwargs (flatten (seq props))
        [_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        ; https://github.com/medfreeman/remark-generic-extensions/issues/45
        label (or-str content (name rel))]
    (apply (:anchor ctx) rel path ctx label kwargs)))

(defn cell [content argument props ctx]
  (let [kwargs (flatten (seq (keywordize-keys props)))
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (apply (:cell ctx) path ctx nil kwargs)))

(defn ^:deprecated -table [content argument {:keys [class] :as props} ctx]
  (hypercrud.ui.table/Table ctx))

(defn result [content argument {:keys [class] :as props} ctx]
  (let [f (read-eval-with-bindings content)]
    [:div.unp (hypercrud.ui.result/result ctx f)]))

(letfn [(keyfn [relation] (hash (map #(or (:db/id %) %) relation)))]
  (defn list- [content argument {:keys [class] :as props} ctx]
    [:div {:class (classes class)}
     (->> (:relations ctx)
          (r/unsequence keyfn)
          (map (fn [[relation k]]
                 ^{:key k} [markdown- content (context/relation ctx relation)]))
          (doall))]))

(defn value [content argument props ctx]
  (let [content (read-eval-with-bindings content)
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (fragment path ((:value ctx) path ctx content))))

(def whitelist-reagent
  ; Div is not needed, use it with block syntax and it hits React.createElement and works
  ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
  {"span" (fn [content argument props ctx]
            [:span (dissoc props :children) content])
   "p" (fn [content argument props ctx]
         [:div (-> props
                   (dissoc :children)
                   (update :class #(classes "p" %)))
          (:children props)])
   "CodeEditor" (fn [content argument props ctx]
                  [contrib.ui/code content #() props])
   "block" (fn [content argument props ctx]
             [:div props [markdown- content]])
   "cljs" eval
   "browse" browse
   "anchor" anchor
   "cell" cell
   "table" -table
   "result" result
   "list" list-
   "value" value
   })

(def remarkInstance (remark/remark whitelist-reagent md-extension))
