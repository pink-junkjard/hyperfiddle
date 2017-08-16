(ns hypercrud.browser.auto-anchor-txfn
  (:require-macros [hypercrud.util.template :as template])
  (:require [clojure.string :as string]
            [hypercrud.compile.eval :as eval]
            [hypercrud.compile.macros :refer [str-and-code']]))


(def auto-tx-fn-lookup
  (letfn [(process-resource [code-str]
            (let [code-str (string/trim code-str)]
              (str-and-code' (eval/eval-str-and-throw code-str) code-str)))]
    {{:m true :fe true :a true} (-> (template/load-resource "auto-txfn/mt-fet-at.edn")
                                    process-resource)
     {:m true :fe true :a false} (-> (template/load-resource "auto-txfn/mt-fet-af.edn")
                                     process-resource)

     {:m true :fe false :a true} nil
     {:m true :fe false :a false} nil

     {:m false :fe true :a true} nil
     {:m false :fe true :a false} nil

     {:m false :fe false :a true} nil
     {:m false :fe false :a false} nil
     }))

(defn auto-txfn [anchor]
  (get auto-tx-fn-lookup
       {:m (or (:anchor/managed? anchor) false)
        :fe (not (nil? (:anchor/find-element anchor)))
        :a (not (nil? (:anchor/attribute anchor)))}))
