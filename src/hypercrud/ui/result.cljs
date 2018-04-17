(ns hypercrud.ui.result
  (:require [contrib.css :refer [classes]]
            [contrib.string :refer [or-str]]
            [contrib.macros :refer [str-and-code]]
            [contrib.reactive :as reactive]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.markdown-rendered :refer [markdown]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


(defn ^:export result "f is parameterized by markdown e.g. !result[hypercrud.ui.result/list]()"
  [ctx & [f]]                                               ; should have explicit mapcat, like markdown.
  ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
  ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
  ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
  (let [f (or f
              (if (:relations ctx)
                table/Table
                form/Relation))]
    (f ctx)))

(def ^:export fiddle
  (str-and-code
    (fn [ctx & [class]]
      [:div {:class (contrib.css/classes "auto-result" class)} ; auto-result ?
       [:h3 (some-> ctx :hypercrud.browser/fiddle deref :fiddle/ident name)]
       [hypercrud.ui.control.markdown-rendered/markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
       (hypercrud.ui.control.link-controls/render-nav-cmps [] false ctx :class "hyperfiddle-link-index")
       (let [content (contrib.string/or-str @(contrib.reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/markdown]) "!result[]")]
         [hypercrud.ui.control.markdown-rendered/markdown content ctx])
       (hypercrud.ui.control.link-controls/render-inline-links [] false ctx)])))

(defn fiddle-xray [ctx class]
  [:div {:class (classes "auto-result" class)}              ; auto-result ?
   [:h3 (some-> ctx :hypercrud.browser/fiddle deref :fiddle/ident name)]
   [markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
   (link-controls/render-nav-cmps [] false ctx :class "hyperfiddle-link-index")
   (result ctx)
   (link-controls/render-inline-links [] false ctx)])
