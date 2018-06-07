(ns hypercrud.ui.table
  (:require [contrib.css :refer [css-slugify classes]]
            [contrib.reactive :as r]
            [contrib.reagent :refer [fragment]]
            [hypercrud.browser.system-link :refer [system-link?]]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]
            [hypercrud.ui.auto-control :refer [auto-control control-props]]
            [hypercrud.ui.connection-color :as connection-color]
            [hypercrud.ui.control.link-controls :as link-controls]
            [hypercrud.ui.label :refer [label]]
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
  ([ctx] (Field nil ctx nil))
  ([?f ctx props]
   (let [[i a] [(:fe-pos ctx) (:hypercrud.browser/attribute ctx)]
         path (remove nil? [i a])]
     (if (:relation ctx)
       [:td {:class (classes (:class props) "hyperfiddle-table-cell" "truncate")
             :style {:border-color (if i (border-color ctx))}}

        ; cell value and dependent=true attribute links. Not element links.
        ; This cell is empty if
        (if a
          (fragment :_
                    ; Widget is responsible for all the links in the cell position.
                    #_(link-controls/anchors path true ctx link/options-processor)
                    #_(link-controls/iframes path true ctx link/options-processor)
                    ; todo unsafe execution of user code: control
                    [(or ?f (auto-control ctx)) @(:value ctx) ctx (merge (control-props ctx) props)]))

        (if i
          ; dependent=true element links
          (fragment :_
                    (link-controls/anchors path true ctx link/options-processor)
                    (link-controls/iframes path true ctx link/options-processor)))]

       [:th {:class (classes "hyperfiddle-table-cell"
                             (css-slugify (:hypercrud.browser/attribute ctx))
                             (if (and i (sortable? ctx path)) "sortable")
                             (some-> (sort-direction ctx) name))
             :style {:background-color (connection-color/connection-color ctx)}
             :on-click (r/partial toggle-sort! ctx path)}
        (if a
          ; label and dependent=false attribute links
          [label ctx])

        (if (and i (not a))
          ; dependent=false element links
          (fragment :_
                    (link-controls/anchors path false ctx link/options-processor)
                    (link-controls/iframes path false ctx link/options-processor)))

        (if (not i)
          ; dependent=false naked links
          (fragment :_
                    (link-controls/anchors path false ctx link/options-processor)
                    (link-controls/iframes path false ctx link/options-processor)))]))))
