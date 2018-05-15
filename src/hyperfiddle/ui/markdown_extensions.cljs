(ns hyperfiddle.ui.markdown-extensions
  (:require
    [clojure.walk :refer [keywordize-keys]]
    [contrib.css :refer [classes]]
    [contrib.data :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.string :refer [memoized-safe-read-edn-string or-str]]
    [contrib.ui]
    [hypercrud.browser.context :as context]
    [hyperfiddle.eval :refer [read-eval-with-bindings]]))


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
  (let [content (read-eval-with-bindings content)
        kwargs (flatten (seq (keywordize-keys props)))
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (apply (:cell ctx) path ctx content :class "unp" kwargs)))

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
                 ^{:key k} [hyperfiddle.ui/markdown content (context/relation ctx relation)]))
          (doall))]))

(defn value [content argument props ctx]
  (let [content (read-eval-with-bindings content)
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (fragment path ((:value ctx) path ctx content))))

(def extensions
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
             [:div props [hyperfiddle.ui/markdown content]])
   "cljs" eval
   "browse" browse
   "anchor" anchor
   "cell" cell
   "table" -table
   "result" result
   "list" list-
   "value" value
   })
