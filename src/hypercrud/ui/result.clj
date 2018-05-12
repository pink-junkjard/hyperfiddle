(ns hypercrud.ui.result
  (:require
    [contrib.string :refer [slow-pprint-str]]
    [cuerdas.core :as str]))


(def expr
  '[:div {:class class}
    (let [fiddle @(:hypercrud.browser/fiddle ctx)]
      [hyperfiddle.ui/markdown (:fiddle/markdown fiddle) ctx])])

(def expr-manually-formatted
  ; Format this manually because:
  ; - Syntax quote will expand @ into `(clojure.core/deref ..)`
  ; - pretty printers suck at clojure, even the slow one
  ; embedded newline lets this pass the cursive clojure formatter
  "
[:div {:class class}
 (let [fiddle @(:hypercrud.browser/fiddle ctx)]
   [hyperfiddle.ui/markdown (:fiddle/markdown fiddle) ctx])]")

(defmacro -build-fiddle []                                  ; Pretty print at compile-time
  `(with-meta (~'fn ~'[ctx & [class]] ~expr)
              {:expr-str ~(str/ltrim expr-manually-formatted "\n")}))
