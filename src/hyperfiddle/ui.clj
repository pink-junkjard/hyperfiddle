(ns hyperfiddle.ui
  (:require
    [contrib.pprint :refer [slow-pprint-str]]
    [cuerdas.core :as str]))


(def expr
  '(let [{:keys [:hypercrud.browser/fiddle]} ctx]
     [:div props
      [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
      [hyperfiddle.ui/result val ctx props]]))

(def expr-manually-formatted
  ; Format this manually:
  ; - Syntax quote will expand @ into `(clojure.core/deref ..)`
  ; - pretty printers suck at clojure, even the slow one
  ; embedded newline lets this pass the cursive clojure formatter
  "
(let [{:keys [:hypercrud.browser/fiddle]} ctx]
  [:div props
   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
   [hyperfiddle.ui/result val ctx props]])")

(defmacro -build-fiddle []                                  ; Pretty print at compile-time
  `(with-meta (~'fn ~'[val ctx props] ~expr)
              {:expr-str ~(str/ltrim expr-manually-formatted "\n")}))
