(ns hypercrud.ui.result
  (:require-macros [hypercrud.ui.result :refer [-build-fiddle]])
  (:require [contrib.css :refer [classes]]
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

(def ^:export fiddle (-build-fiddle))

(defn fiddle-xray [ctx class]
  [:div {:class (classes "auto-result" class)}              ; auto-result ?
   [:h3 (some-> ctx :hypercrud.browser/fiddle deref :fiddle/ident name)]
   [markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
   (link-controls/anchors [] false ctx :class "hyperfiddle-link-index")
   (result ctx)
   (link-controls/iframes [] false ctx)])
