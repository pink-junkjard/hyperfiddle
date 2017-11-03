(ns hypercrud.ui.edn
  (:require [hypercrud.util.string :refer [safe-read-edn-string]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.ui.code-editor :as code-editor]
            [cats.monad.either :as either]))


; Must validate since invalid edn means there's no value to stage.
; Code editors are different since you are permitted to stage broken code (and see the error and fix it)
(defn edn* [code-control value change! props]
  (let [change! (fn [user-edn-str]
                  (-> (safe-read-edn-string user-edn-str)
                      (either/branch
                        (fn [e] (js/console.warn (pr-str e)) nil)  ; report error
                        (fn [v] (change! v)))))]
    [code-control props (pprint-str value) change!])) ; not reactive


(defn edn-block [value change! props]
  (edn* code-editor/code-block value change! (assoc props :mode "clojure")))


(defn edn-inline-block [value change! props]
  (edn* code-editor/code-inline-block value change! (assoc props :mode "clojure")))
