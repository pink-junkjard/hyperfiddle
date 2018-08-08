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

(defn- read-path [s]
  (either/branch
    (memoized-safe-read-edn-string (str "[" s "]"))
    #(do (timbre/error %) nil)                              ; too late to report anything to the dev
    identity))

(defn same-path-as? [path link]
  (= path (read-path (:link/path link))))

(defn draw-link? "Full semantics unclear" [path link]
  (let [classes (->> (:link/class link) (map read-path) (remove nil?) (into #{}))]
    (if (empty? classes)
      (= path (read-path (:link/path link)))
      (contains? classes path))))

(defn links-at [path links-ref]
  (filter (partial same-path-as? path) @links-ref))

(defn select-all-at-path [ctx rel]
  (->> (filter #(= (:link/rel %) rel) @(:hypercrud.browser/links ctx))
       (filter (partial same-path-as? (:hypercrud.browser/path ctx)))))

(defn rel->link "doesn't validate" [rel ctx]
  (first (select-all-at-path ctx rel)))

(defn same-class-as? [class {classes :link/class}]
  (boolean ((set classes) class)))

(defn select-all "Find the closest match. Can it search parent scopes for :options ?"
  ([ctx rel] {:pre [rel]}
   (->> @(:hypercrud.browser/links ctx)
        (filter #(= rel (:link/rel %)))))
  ([ctx rel ?class]
   (->> (select-all ctx rel)
        (filter (fn [link]
                  (if ?class
                    (same-class-as? ?class link)
                    true)))))
  ([ctx rel ?class ?path]
   (->> (select-all ctx rel ?class)
        (filter (fn [link]
                  (if ?path
                    (draw-link? ?path link)
                    true))))))

(comment
  ; Maybe this shouldn't exist, the caller should validate?
  (defn select-one [ctx rel & [?class]]
    (first (select-all ctx rel ?class))))

(let [memoized-eval-props (memoize eval/safe-eval-string)]
  (defn eval-hc-props [props-str ctx]
    (if (and (string? props-str) (not (string/blank? props-str)))
      (cats/bind (memoized-eval-props props-str)
                 (fn [f-or-v]
                   (if (fn? f-or-v)
                     (try-either (f-or-v ctx))
                     (either/right f-or-v))))
      (either/right nil))))
