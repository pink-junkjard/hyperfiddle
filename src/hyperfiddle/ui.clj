(ns hyperfiddle.ui
  (:require
    [contrib.pprint :refer [slow-pprint-str]]
    [cuerdas.core :as str]))


(def expr
  '(let [{:keys [:hypercrud.browser/data
                 :hypercrud.browser/fiddle]} ctx]
     [:div {:class class}
      [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
      [hyperfiddle.ui/result ctx]]))

(def expr-manually-formatted
  ; Format this manually:
  ; - Syntax quote will expand @ into `(clojure.core/deref ..)`
  ; - pretty printers suck at clojure, even the slow one
  ; embedded newline lets this pass the cursive clojure formatter
  "
(let [{:keys [:hypercrud.browser/data
              :hypercrud.browser/fiddle]} ctx]
  [:div {:class class}
   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
   [hyperfiddle.ui/result ctx]])")

(defmacro -build-fiddle []                                  ; Pretty print at compile-time
  `(with-meta (~'fn ~'[ctx & [class]] ~expr)
              {:expr-str ~(str/ltrim expr-manually-formatted "\n")}))
