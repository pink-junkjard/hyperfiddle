(ns hypercrud.ui.table
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [hypercrud.browser.system-link :refer [system-link?]]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.label :refer [auto-label]]
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

(defn border-color [ctx]
  (let [shadow-link @(r/fmap system-link? (r/cursor (:cell-data ctx) [:db/id]))]
    (if-not shadow-link (connection-color/connection-color ctx))))

(defn Field "Form fields are label AND value. Table fields are label OR value."
  [f ctx props]
  (let [{:keys [hypercrud.browser/field
                hypercrud.browser/attribute]} ctx
        [i a] [(:fe-pos ctx) attribute]
        path (remove nil? [i a])]
    (if (:relation ctx)
      [:td {:class (classes #_"field" "hyperfiddle-table-cell" (:class props) "truncate")
            :style {:border-color (if i (border-color ctx))}}
       ; todo unsafe execution of user code: control
       ; todo give the right value for this path
       [f (some-> ctx :value deref) ctx props]]
      [:th {:class (classes #_"field" "hyperfiddle-table-cell" (:class props)
                                      (if (and i (sortable? ctx path)) "sortable") ; hoist
                                      (some-> (sort-direction ctx) name)) ; hoist
            :style {:background-color (connection-color/connection-color ctx)}
            :on-click (r/partial toggle-sort! ctx path)}
       ((or (:label-fn props) auto-label) field ctx props)])))
