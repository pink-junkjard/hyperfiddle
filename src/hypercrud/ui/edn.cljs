(ns hypercrud.ui.edn
  (:require [hypercrud.util.string :refer [safe-read-string]]
            [hypercrud.util.core :refer [pprint-str]]
            [hypercrud.ui.code-editor :as code-editor]
            [cats.monad.either :as either]))


(defn edn* [value change! props]
  (let [change! (fn [user-edn-str]
                  (-> (safe-read-string user-edn-str)
                      (either/branch
                        (fn [e] (js/alert (pr-str e)) nil)  ; report error
                        #(change! %))))])
  [code-editor/code-inline-block props (pprint-str value) change!]) ; not reactive
