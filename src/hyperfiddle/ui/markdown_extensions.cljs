(ns hyperfiddle.ui.markdown-extensions
  (:require
    [cats.core :refer [fmap mlet return]]
    [cats.monad.either :as either]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string :refer [memoized-safe-read-edn-string or-str]]
    [contrib.ui.remark :as remark]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hypercrud.ui.error :refer [error-comp]]
    [hyperfiddle.data :as data]
    [hyperfiddle.ui]
    [taoensso.timbre :as timbre]))


(defn a [content argument props ctx]
  [:a (merge {:href argument} (dissoc props :children))
   ; backwards compat with vanilla markdown anchors
   (or (:children props) content)])

; mutual recursion, it would be letfn if wasn't react components
(declare markdown)

(let [memoized-safe-eval (memoize eval/safe-eval-string+)]
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
                   [contrib.ui/code {:value content :read-only true}])
                 [contrib.ui/code (assoc props :value content)]))

       "render" (fn [content argument props ctx]
                  (->> (memoized-safe-eval (str "(fn [ctx] \n" content "\n)"))
                       (fmap (fn [f] (f ctx)))
                       (unwrap #(timbre/warn %))))

       "f" (fn [content argument props ctx]
             (let [f (some->> content memoized-safe-eval (unwrap #(timbre/warn %)))
                   val (some->> argument memoized-safe-eval (unwrap #(timbre/warn %)))]
               (when f [f val props ctx])))

       "browse" (fn [content argument props ctx]
                  (let [[_ srel sclass] (re-find #"([^ ]*) ?(.*)" argument)
                        rel (some->> srel memoized-safe-eval (unwrap #(timbre/warn %)))
                        class (some->> sclass memoized-safe-eval (unwrap #(timbre/warn %)))
                        ?f (some->> content memoized-safe-eval (unwrap #(timbre/warn %)))]
                    (hyperfiddle.ui/browse rel class ctx ?f props)))

       "live" (fn [content argument props ctx]
                (let [[_ srel spath] (re-find #"([^ ]*) ?(.*)" argument)]
                  (-> (mlet [rel (memoized-safe-read-edn-string srel)
                             class (memoized-safe-read-edn-string spath)
                             fiddle-attrs (memoized-safe-read-edn-string (str "[" content "]"))
                             :let [props (assoc props :hyperfiddle.ide.hf-live/fiddle-attrs fiddle-attrs)]]
                        (return [hyperfiddle.ide.hf-live/browse rel class ctx props]))
                      (either/branch
                        (fn [e] [(error-comp ctx) e])
                        identity))))

       "link" (fn [content argument props ctx]
                (let [[_ rel-s class-s] (re-find #"([^ ]*) ?(.*)" argument)
                      rel (some->> rel-s memoized-safe-eval (unwrap #(timbre/warn %)))
                      ?class (some->> class-s memoized-safe-eval (unwrap #(timbre/warn %)))
                      ; https://github.com/medfreeman/remark-generic-extensions/issues/45
                      label (or-str content (name rel))]
                  (hyperfiddle.ui/link rel ?class ctx label props)))

       "result" (fn [content argument props ctx]
                  (let [ctx (assoc ctx ::unp true)]
                    (if-let [f (some->> content memoized-safe-eval (unwrap #(timbre/warn %)))]
                      [f ctx]
                      (hyperfiddle.ui/result @(:hypercrud.browser/data ctx) ctx (update props :class css "unp")))))
       "value" (fn [content argument props ctx]
                 (let [path (unwrap #(timbre/warn %) (memoized-safe-read-edn-string (str "[" argument "]")))
                       ?f (some->> content memoized-safe-eval (unwrap #(timbre/warn %)))]
                   (hyperfiddle.ui/value path ctx ?f props)))

       "field" (fn [content argument props ctx]
                 (let [path (unwrap #(timbre/warn %) (memoized-safe-read-edn-string (str "[" argument "]")))
                       ?f (some->> content memoized-safe-eval (unwrap #(timbre/warn %)))]
                   (hyperfiddle.ui/field path ctx ?f (-> props
                                                         (update :class css "unp")
                                                         (update :label-fn contrib.eval/ensure-fn)))))

       "table" (letfn [(fields [content props ctx]
                         [[markdown content (assoc ctx ::unp true)]])]
                 (fn [content argument props ctx]
                   [hyperfiddle.ui/table (r/partial fields content props) ctx #_props]))

       "list" (fn [content argument props ctx]
                [:ul props
                 (->> (:hypercrud.browser/data ctx)
                      (r/unsequence data/row-keyfn)
                      (map (fn [[row k]]
                             ^{:key k}
                             [:li [markdown content (context/row ctx row)]]))
                      (doall))])})))
