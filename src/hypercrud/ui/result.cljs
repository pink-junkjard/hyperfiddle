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


(defn ^:export result "f is parameterized by markdown e.g. !result[hypercrud.ui.result/list]()"
  [ctx & [f]]                           ; should have explicit mapcat, like markdown.
  ; This is not a reagent component; it returns a component-or-list-of-components (or nil).
  ; Thus it cannot be used from hiccup syntax. It needs to be wrapped into a :div or a react-fragment.
  ; Which means at that point it might as well return monad and let the wrapper sort out the errors?
  (let [f (or f
              (if (:relations ctx)
                table/Table
                form/Relation))]
    (f ctx)))

(defn ^:deprecated ^:export ident [ctx]                                           ; simplify and inline
  (markdown-rendered* (-> ctx
                          :hypercrud.browser/fiddle
                          (reactive/cursor [:fiddle/ident])
                          (as-> % (reactive/fmap (fnil name :_) %))
                          deref
                          (as-> % (str "### " %)))))

(defn ^:deprecated ^:export doc [ctx]                                             ; simplify and inline
  (markdown-rendered* @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:db/doc])))

(defn ^:export fiddle [ctx & [class]]                       ; should be a string template to inline in userland for editing.
  (let [index-ctx (dissoc ctx :isComponent)]
    [:div {:class (classes "auto-result" class)}            ; auto-result ?
     (ident ctx)
     (doc ctx)
     (link-controls/render-nav-cmps [] false index-ctx :class "hyperfiddle-link-index")
     (let [content @(reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/markdown])]
       (markdown-rendered* (or content "!result[]") ctx))
     (link-controls/render-inline-links [] false index-ctx)]))

(defn fiddle-xray [ctx class]
  (let [index-ctx (dissoc ctx :isComponent)]
    [:div {:class (classes "auto-result" class)}            ; auto-result ?
     (ident ctx)
     (doc ctx)
     (link-controls/render-nav-cmps [] false index-ctx :class "hyperfiddle-link-index")
     (result ctx)
     (link-controls/render-inline-links [] false index-ctx)]))

(def ^:deprecated ^:export result-renderer result)
(def ^:deprecated ^:export fiddle-markdown fiddle)
(def ^:deprecated ^:export view fiddle)
