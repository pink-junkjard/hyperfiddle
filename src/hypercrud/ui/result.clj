(ns hypercrud.ui.result
  (:require
    [contrib.string :refer [slow-pprint-str]]))


(defmacro -build-fiddle []
  (let [expr '[:div {:class (contrib.css/classes "auto-result" class)} ; auto-result ?
               [:h3 (some-> ctx :hypercrud.browser/fiddle deref :fiddle/ident name)]
               [hypercrud.ui.control.markdown-rendered/markdown (-> ctx :hypercrud.browser/fiddle deref :db/doc)]
               (hypercrud.ui.control.link-controls/anchors [] false ctx :class "hyperfiddle-link-index")
               (let [content (contrib.string/or-str @(contrib.reactive/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/markdown]) "!result[]")]
                 [hypercrud.ui.control.markdown-rendered/markdown content ctx])
               (hypercrud.ui.control.link-controls/iframes [] false ctx)]]
    `(with-meta (~'fn ~'[ctx & [class]] ~expr)
                {:expr-str ~(slow-pprint-str expr)})))
