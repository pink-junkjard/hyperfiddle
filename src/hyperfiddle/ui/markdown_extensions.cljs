(ns hyperfiddle.ui.markdown-extensions
  (:require
    [clojure.walk :refer [keywordize-keys]]
    [contrib.css :refer [classes]]
    [contrib.data :refer [unwrap fix-arity]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment with-react-context]]
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
        f? (read-eval-with-bindings content)]
    (apply (:browse ctx) rel path ctx f? kwargs)))

(defn anchor [content argument props ctx]
  (let [kwargs (flatten (seq props))
        [_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        ; https://github.com/medfreeman/remark-generic-extensions/issues/45
        label (or-str content (name rel))]
    (apply (:anchor ctx) rel path ctx label kwargs)))

(defn field [content argument props ctx]
  (let [?f (read-eval-with-bindings content)
        props (keywordize-keys props)
        props (clojure.set/rename-keys props {:className :class})
        props (update props :class classes "unp")  #_"fix font size"
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (apply (:cell ctx) path ctx (if ?f (fn control [value ctx props]
                                         [with-react-context
                                          {:ctx ctx :props props}
                                          ; the whole point of the gymnastics is to apply ?f as arity-1 (so `str` works)
                                          [?f #_(wrap-naked-string :div ?f) value]]))
           (flatten (seq props)))))

(defn ^:deprecated -table [content argument {:keys [class] :as props} ctx]
  [:div.unp (hypercrud.ui.table/Table ctx)])

(defn result [content argument props ctx]
  (let [?f (read-eval-with-bindings content)]
    [:div.unp (hypercrud.ui.result/result ctx ?f)]))

(letfn [(keyfn [relation] (hash (map #(or (:db/id %) %) relation)))]
  (defn list- [content argument props ctx]
    [:ul.unp props
     (->> (:relations ctx)
          (r/unsequence keyfn)
          (map (fn [[relation k]]
                 ^{:key k} [:li [hyperfiddle.ui/markdown content (context/relation ctx relation)]]))
          (doall))]))

(defn value [content argument props ctx]
  (let [?f (read-eval-with-bindings content)
        props (keywordize-keys props)
        path (into [true] (unwrap (memoized-safe-read-edn-string (str "[" argument "]"))))]
    (apply (:value ctx) path ctx
           (if ?f (fn control [value ctx props]
                    [with-react-context
                     {:ctx ctx :props props}
                     ; the whole point of the gymnastics is to apply ?f as arity-1 (so `str` works)
                     [?f #_(wrap-naked-string :div ?f) value]]))
           (flatten (seq props)))))

(def extensions
  ; Div is not needed, use it with block syntax and it hits React.createElement and works
  ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
  {; Replace default elements with our classes, hacks
   ;"ul" (fn [content argument props ctx]
   ;       [:ul.p (dissoc props :children) (:children props)])
   "li" (fn [content argument props ctx]
          [:li.p (dissoc props :children) (:children props)])
   "p" (fn [content argument props ctx]
         [:div.p (dissoc props :children) (:children props)])
   "span" (fn [content argument props ctx]
            [:span (dissoc props :children) content])
   "block" (fn [content argument props ctx]
             [:div props [hyperfiddle.ui/markdown content]])
   "CodeEditor" (fn [content argument props ctx]
                  [contrib.ui/code content #() props])
   "cljs" eval
   "browse" browse
   "anchor" anchor
   "cell" field                                             ; legacy
   "field" field
   "table" -table
   "result" result
   "list" list-
   "value" value
   })
