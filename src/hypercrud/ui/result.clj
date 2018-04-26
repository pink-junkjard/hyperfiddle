(ns hypercrud.ui.result
  (:require
    [contrib.string :refer [slow-pprint-str]]))


(defmacro -build-fiddle []                                  ; Pretty print at compile-time
    (let [expr '[:div {:class class}
                 [:h3 (some-> ctx :hypercrud.browser/fiddle deref :fiddle/ident name)]
                 (hypercrud.ui.control.link-controls/anchors [] false ctx :class "hyperfiddle-link-index")
                 [hypercrud.ui.control.markdown-rendered/markdown (some-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]
                 (hypercrud.ui.control.link-controls/iframes [] false ctx)]]
      `(with-meta (~'fn ~'[ctx & [class]] ~expr)
                  {:expr-str ~(slow-pprint-str expr)})))
