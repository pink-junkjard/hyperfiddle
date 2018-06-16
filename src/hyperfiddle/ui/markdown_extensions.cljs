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
  (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        f? (read-eval-with-bindings content)]
    (hyperfiddle.ui/browse rel path ctx f? props)))

(defn link [content argument props ctx]
  (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
        rel (unwrap (memoized-safe-read-edn-string srel))
        path (unwrap (memoized-safe-read-edn-string (str "[" spath "]")))
        ; https://github.com/medfreeman/remark-generic-extensions/issues/45
        label (or-str content (name rel))]
    (hyperfiddle.ui/link rel path ctx label props)))

(defn control-arity-1-with-context "partial the ?f"
  [?f value ctx props]
  [with-react-context {:ctx ctx :props props} [?f value]])

(defn field [content argument props ctx]
  (let [props (-> props
                  (dissoc :children)
                  (clojure.set/rename-keys {:className :class})
                  (update :class css "unp") #_"fix font size")
        path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
        ?f (read-eval-with-bindings content)
        ?f (if ?f (r/partial control-arity-1-with-context ?f))]
    (hyperfiddle.ui/field path ctx ?f props)))

(defn list- [content argument props ctx]
  [:ul props
   (->> (:relations ctx)
        (r/unsequence hf/relation-keyfn)
        (map (fn [[relation k]]
               ; set ::unp to suppress
               ^{:key k} [:li [hyperfiddle.ui/markdown content (context/relation ctx relation)]]))
        (doall))])

(defn value [content argument props ctx]
  (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
        ?f (read-eval-with-bindings content)
        ?f (if ?f (r/partial control-arity-1-with-context ?f))]
    (hyperfiddle.ui/value path ctx ?f props)))

(def extensions
  ; Div is not needed, use it with block syntax and it hits React.createElement and works
  ; see https://github.com/medfreeman/remark-generic-extensions/issues/30
  {; Replace default elements with our classes, hacks
   "li" (fn [content argument props ctx]
          [:li.p (dissoc props :children) (:children props)])
   "p" (fn [content argument props ctx]
         ; Really need a way to single here from below, to get rid of div.p
         ; So that means signalling via this :children value
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
              (hyperfiddle.ui/result (assoc ctx ::unp true)
                                     (read-eval-with-bindings content)
                                     (update props :class css "unp")))
   "value" value                                            ; uses relation to draw just value
   "field" field                                            ; uses relation to draw label and value
   "table" (letfn [(form [content ctx]
                     [[hyperfiddle.ui/markdown content (assoc ctx ::unp true)]])]
             (fn [content argument props ctx]
               [hyperfiddle.ui/table (r/partial form content) hf/sort-fn ctx]))
   "list" list-                                             ; renders ul/li, loops over relations

   ; How can list collapse into result through a higher order fn? Would need two fns, wrapper and inner...
   ; This is a similar question to a parameterized table renderer which is a 2D parameterized form/field renderer.

   ; legacy
   "cell" field
   })
