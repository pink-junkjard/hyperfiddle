(ns hypercrud.browser.link
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [contrib.eval :as eval]
    [contrib.string :refer [memoized-safe-read-edn-string]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as string]
    [taoensso.timbre :as timbre]))


(defn popover-link? [link] (boolean (:link/managed? link)))

(defn read-path [s]
  {:post []}
  (let [v
        (either/branch
          (memoized-safe-read-edn-string (str "[" s "]"))
          #(do (timbre/error %) nil)                        ; too late to report anything to the dev
          identity)]
    (assert (empty? (filter #{:head :body} v)) v)
    v))

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))

(defn links-at [path links-ref]
  (filter (partial same-path-as? path) @links-ref))

(let [memoized-eval-props (memoize eval/safe-eval-string)]
  (defn eval-hc-props [props-str ctx]
    (if (and (string? props-str) (not (string/blank? props-str)))
      (cats/bind (memoized-eval-props props-str)
                 (fn [f-or-v]
                   (if (fn? f-or-v)
                     (try-either (f-or-v ctx))
                     (either/right f-or-v))))
      (either/right nil))))
