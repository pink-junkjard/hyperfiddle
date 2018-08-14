(ns hyperfiddle.ui.markdown-extensions
  (:require
    [cats.core :refer [fmap]]
    [contrib.css :refer [css]]
    [contrib.data :refer [unwrap]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string or-str]]
    [contrib.ui.remark :as remark]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui]))


(defn a [content argument props ctx]
  [:a (merge {:href argument} (dissoc props :children))
   ; backwards compat with vanilla markdown anchors
   (or (:children props) content)])

; mutual recursion, it would be letfn if wasn't react components
(declare markdown)

(let [memoized-safe-eval (memoize eval/safe-eval-string)]
  (def ^:export markdown
    (remark/remark!

      ; Content is text, or more markdown, or code
      ; Argument is semantic: a url, or a hyperfiddle ident (or a second optional content? Caption, row-renderer)

      {"li" (fn [content argument props ctx]
              [:li.p (dissoc props :children) (:children props)])

       "p" (fn [content argument props ctx]
             ; Really need a way to single here from below, to get rid of div.p
             ; So that means signalling via this :children value
             (if (::unp ctx)
               (js/reactCreateFragment #js {"_" (:children props)})
               [:div.p (dissoc props :children) (:children props)]))

       "span" (fn [content argument props ctx]
                [:span (remark/adapt-props props)
                 [markdown content (assoc ctx ::unp true)]])

       "a" hyperfiddle.ui.markdown-extensions/a

       ; Is this comment true?::
       ;   Div is not needed, use it with block syntax and it hits React.createElement and works
       ;   see https://github.com/medfreeman/remark-generic-extensions/issues/30

       "block" (fn [content argument props ctx]
                 ; Should presence of argument trigger a figure and caption?
                 [:div props [markdown content (assoc ctx ::unp true)]])

       ; This is a custom markdown extension example.
       "figure" (fn [content argument props ctx]
                  [:figure.figure props
                   [markdown content (assoc ctx ::unp true)] ; it's an image or pre or other block element
                   [:figcaption.figure-caption [markdown argument (assoc ctx ::unp true)]]])

       "pre" (fn [content argument props ctx]
               ; detect ``` legacy syntax, no props or argument
               (if-let [children (:children props)]
                 ; Remark generates pre>code; deep inspect and rip out the content
                 ; Don't hook :code because that is used by inline snippets
                 (let [content (goog.object/getValueByKeys children 0 "props" "children" 0)
                       content (str/rtrim content "\n") #_"Remark yields an unavoidable newline that we don't want"]
                   [contrib.ui/code content #() {:read-only true}])
                 [contrib.ui/code content #() props]))

       "render" (fn [content argument props ctx]
                  (->> (memoized-safe-eval (str "(fn [ctx] \n" content "\n)"))
                       (fmap (fn [f] (f ctx)))
                       (unwrap)))

       "f" (fn [content argument props ctx]
             (let [f (unwrap (memoized-safe-eval content))
                   val (unwrap (memoized-safe-read-edn-string argument))]
               (when f [f val props ctx])))

       "browse" (fn [content argument props ctx]
                  (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)
                        rel (unwrap (memoized-safe-read-edn-string srel))
                        class (unwrap (memoized-safe-read-edn-string spath))
                        f? (unwrap (memoized-safe-eval content))]
                    (hyperfiddle.ui/browse rel class ctx f? props)))

       "link" (fn [content argument props ctx]
                (let [[_ rel-s class-s] (re-find #"([^ ]*) ?(.*)" argument)
                      rel (unwrap (memoized-safe-read-edn-string rel-s))
                      class (unwrap (memoized-safe-read-edn-string class-s))
                      ; https://github.com/medfreeman/remark-generic-extensions/issues/45
                      label (or-str content (name rel))]
                  (hyperfiddle.ui/link rel class ctx label props)))

       "result" (fn [content argument props ctx]
                  (let [ctx (assoc ctx ::unp true)]
                    (if-let [f (unwrap (memoized-safe-eval content))]
                      [f ctx]
                      (hyperfiddle.ui/result ctx (update props :class css "unp")))))
       "value" (fn [content argument props ctx]
                 (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                       ?f (some->> (unwrap (memoized-safe-eval content)))]
                   (hyperfiddle.ui/value path ctx ?f props)))

       "field" (fn [content argument props ctx]
                 (let [path (unwrap (memoized-safe-read-edn-string (str "[" argument "]")))
                       ?f (some->> (unwrap (memoized-safe-eval content)))]
                   (hyperfiddle.ui/field path ctx ?f (update props :class css "unp"))))

       "table" (letfn [(form [content ctx]
                         [[markdown content (assoc ctx ::unp true)]])]
                 (fn [content argument props ctx]
                   [hyperfiddle.ui/table (r/partial form content) ctx #_props]))

       "list" (fn [content argument props ctx]
                [:ul props
                 (->> (:hypercrud.browser/data ctx)
                      (r/unsequence data/row-keyfn)
                      (map (fn [[relation k]]
                             ^{:key k} [:li [markdown content (context/body ctx relation)]]))
                      (doall))])})))
