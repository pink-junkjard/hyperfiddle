(ns hyperfiddle.ui.markdown-extensions
  (:require
    [clojure.walk :refer [keywordize-keys]]
    [contrib.css :refer [css]]
    [contrib.data :refer [unwrap fix-arity]]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment with-react-context]]
    [contrib.string :refer [memoized-safe-read-edn-string or-str]]
    [contrib.ui]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hyperfiddle.eval :refer [read-eval-with-bindings]]
    [hyperfiddle.data :as hf]))


(defn eval [content argument props ctx]
  (read-eval-with-bindings content ctx))

(defn browse [content argument props ctx]
  (let [kwargs (flatten (seq props))
        [_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        f? (read-eval-with-bindings content)]
    (apply hyperfiddle.ui/browse rel path ctx f? kwargs)))

(defn link [content argument props ctx]
  (let [kwargs (flatten (seq props))
        [_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        ; https://github.com/medfreeman/remark-generic-extensions/issues/45
        label (or-str content (name rel))]
    (apply hyperfiddle.ui/link rel path ctx label kwargs)))

(defn field [content argument props ctx]
  (let [?f (read-eval-with-bindings content)
        props (-> props
                  keywordize-keys
                  (dissoc :children)
                  (clojure.set/rename-keys {:className :class})
                  (update :class css "unp") #_"fix font size")
        path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
        ?f (if ?f (fn control [value ctx props]
                    [with-react-context
                     {:ctx ctx :props props}
                     ; the whole point of the gymnastics is to apply ?f as arity-1 (so `str` works)
                     [?f #_(wrap-naked-string :div ?f) value]]))]
    (apply hyperfiddle.ui/field path ctx ?f (flatten (seq props)))))

(defn list- [content argument props ctx]
  [:ul.unp props
   (->> (:relations ctx)
        (r/unsequence hf/relation-keyfn)
        (map (fn [[relation k]]
               ^{:key k} [:li [hyperfiddle.ui/markdown content (context/relation ctx relation)]]))
        (doall))])

(defn value [content argument props ctx]
  (let [?f (read-eval-with-bindings content)
        props (keywordize-keys props)
        path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))]
    (apply (:hyperfiddle.ui/value ctx) path ctx
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
   "li" (fn [content argument props ctx]
          [:li.p (dissoc props :children) (:children props)])
   "p" (fn [content argument props ctx]
         (if (::unp ctx)
           (js/reactCreateFragment #js {"_" (:children props)})
           [:div.p (dissoc props :children) (:children props)]))
   "span" (fn [content argument props ctx]
            [:span (dissoc props :children) content])
   "block" (fn [content argument props ctx]
             [:div props [hyperfiddle.ui/markdown content]])
   "pre" (fn [content argument props ctx]
           ; Remark generates pre>code; deep inspect and rip out the content
           ; Don't hook :code because that is used by inline snippets
           (let [content (-> props :children (goog.object/getValueByKeys 0 "props" "children" 0)) ; get(props, kw('children'))[0].props.children[0]
                 content (str/rtrim content "\n") #_"Remark yields an unavoidable newline that we don't want"]
             ; No way to get props here from userland
             [contrib.ui/code-block {:read-only true} content #()]))
   "CodeEditor" (fn [content argument props ctx]
                  [contrib.ui/code-block props content #()])

   "cljs" eval                                              ; not quite eval, really !reagent bc it must return hiccup

   ; browse, anchor and result are probably the same thing – "render named thing"
   "browse" browse
   "anchor" link

   "result" (fn [content argument props ctx]
              [:div.unp (hyperfiddle.ui/result ctx (read-eval-with-bindings content))])
   "value" value                                            ; uses relation to draw just value
   "field" field                                            ; uses relation to draw label and value
   "table" (letfn [(form [content ctx]
                     [[hyperfiddle.ui/markdown content ctx]])]
             (fn [content argument props ctx]
               [hyperfiddle.ui/table (r/partial form content) hf/sort-fn ctx]))
   "list" list-                                             ; renders ul/li, loops over relations

   ; How can list collapse into result through a higher order fn? Would need two fns, wrapper and inner...
   ; This is a similar question to a parameterized table renderer which is a 2D parameterized form/field renderer.

   ; legacy
   "cell" field
   })
