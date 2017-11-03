(ns hypercrud.ui.edn
  (:require [hypercrud.util.string :refer [safe-read-cljs-string]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.ui.code-editor :as code-editor]
            [cats.monad.either :as either]))


; Must validate since invalid edn means there's no value to stage.
; Code editors are different since you are permitted to stage broken code (and see the error and fix it)
(defn edn* [value change! props]
  (let [change! (fn [user-edn-str]
                  (-> (safe-read-cljs-string user-edn-str)  ; not the right validator - should be edn
                      (either/branch
                        (fn [e] (js/alert (pr-str e)) nil)  ; report error
                        #(change! %))))])
  [code-editor/code-inline-block props (pprint-str value) change!]) ; not reactive
