(ns hyperfiddle.ui.sort
  (:require [contrib.css :refer [css-slugify css]]
            [contrib.reactive :as r]
            [hypercrud.browser.link :as link]
            [hyperfiddle.data :as hf]))


; sorting currently breaks click handling in popovers
(defn links-dont-break-sorting? [path ctx]
  (->> @(:hypercrud.browser/links ctx)
       (filter (link/same-path-as? path))
       (remove :link/dependent?)
       (link/options-processor)
       (not-any? link/popover-link?)))

(defn sortable? [ctx path]
  (and (hf/attr-sortable? @(:hypercrud.browser/find-element ctx)
                          (:attribute (:hypercrud.browser/field ctx))
                          ctx)
       @(r/track links-dont-break-sorting? path ctx)))

(defn sort-direction [ctx]
  (let [[sort-fe-pos sort-attr direction] @(::sort-col ctx)]
    (if (and (= (:fe-pos ctx) sort-fe-pos) (= sort-attr (:attribute (:hypercrud.browser/field ctx))))
      direction)))

(defn toggle-sort! [ctx path]
  (if (sortable? ctx path)
    (reset! (::sort-col ctx)
            (case (sort-direction ctx)
              :asc [(:fe-pos ctx) (:attribute (:hypercrud.browser/field ctx)) :desc]
              :desc nil
              [(:fe-pos ctx) (:attribute (:hypercrud.browser/field ctx)) :asc]))))
