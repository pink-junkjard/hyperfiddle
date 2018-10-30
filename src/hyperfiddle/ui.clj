(ns hyperfiddle.ui
  (:require
    [cuerdas.core :as str]))


(def expr
  '(let [{:keys [:hypercrud.browser/fiddle]} ctx]
     [:div props
      [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
      [hyperfiddle.ui/result val ctx {}]
      [hyperfiddle.ui/field [] ctx hyperfiddle.ui/entity-links-iframe (assoc props :label-fn (contrib.reactive/constantly nil))]]))

(def expr-manually-formatted
  ; Format this manually:
  ; - Syntax quote will expand @ into `(clojure.core/deref ..)`
  ; - pretty printers suck at clojure, even the slow one
  ; embedded newline lets this pass the cursive clojure formatter
  "
(let [{:keys [:hypercrud.browser/fiddle]} ctx]
  [:div props
   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
   [hyperfiddle.ui/result val ctx {}]
   [hyperfiddle.ui/field [] ctx hyperfiddle.ui/entity-links-iframe (assoc props :label-fn (contrib.reactive/constantly nil))]])")

(defmacro -build-fiddle []                                  ; Pretty print at compile-time
  `(with-meta (~'fn ~'[val ctx props] ~expr)
              {:expr-str ~(str/ltrim expr-manually-formatted "\n")}))
