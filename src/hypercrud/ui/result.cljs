(ns hypercrud.ui.result
  (:require [cats.core :refer [fmap]]
            [contrib.css :refer [classes]]
            [contrib.data :as util :refer [or-str]]
            [contrib.reactive :as reactive]
            [hypercrud.browser.context :as context]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.control.markdown-rendered :refer [markdown-relation markdown-rendered*]]
            [hypercrud.ui.form :as form]
            [hypercrud.ui.table :as table]))


(defn ^:export result [ctx & [f]]                           ; should have explicit mapcat, like markdown.
  ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
  ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
  ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
  (let [f (or f
              (if (:relations ctx)
                table/Table
                form/Relation))]
    (f ctx)))

(def ^:deprecated ^:export result-renderer result)

(defn ident [ctx]
  (markdown-rendered* (-> ctx
                          :hypercrud.browser/fiddle
                          (reactive/cursor [:fiddle/ident])
                          (as-> % (reactive/fmap (fnil name :_) %))
                          deref
                          (as-> % (str "### " %)))))

(defn doc [ctx]
  (markdown-rendered* @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:db/doc])))

(defn ^:export fiddle-markdown "Call this from your fiddle renderer"
  [ctx & [class]]
  (let [content @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/markdown])]
    (markdown-relation nil content ctx class)))

(defn ^:export view [ctx & [class]]                         ; hyperfiddle.ui/fiddle :docstring true :ident false
  (let [index-ctx (dissoc ctx :isComponent)]
    [:div {:class (classes "auto-result" class)}
     (ident ctx)
     (doc ctx)
     (link-controls/render-nav-cmps [] false index-ctx :class "hyperfiddle-link-index")
     (result ctx)
     (link-controls/render-inline-links [] false index-ctx)]))
