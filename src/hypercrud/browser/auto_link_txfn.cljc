(ns hypercrud.browser.auto-link-txfn
  (:require [cats.monad.either :as either]
            [clojure.string :as string]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.template :as template]
            [hypercrud.browser.context :as context]
            [taoensso.timbre :as timbre]))


(def parent-child (-> (template/load-resource "auto-txfn/mt-fet-at.edn") string/trim))

(defn auto-txfn [link]
  ; tx-fn is not applicable if the link is not managed
  (when (:link/managed? link)
    (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
        (either/branch
          (fn [e] (timbre/error e))
          (fn [path]
            (when (and (:link/create? link) (context/attribute-segment? (last path)))
              parent-child))))))
