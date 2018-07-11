(ns hyperfiddle.ui.sort
  (:require [contrib.css :refer [css-slugify css]]
            [contrib.reactive :as r]
            [hypercrud.browser.context :as context]
            [hypercrud.browser.link :as link]))


; sorting currently breaks click handling in popovers
(defn links-dont-break-sorting? [ctx]
  (->> @(:hypercrud.browser/links ctx)
       (filter (link/same-path-as? (:hypercrud.browser/path ctx)))
       (remove :link/dependent?)
       (link/options-processor)
       (not-any? link/popover-link?)))

(defn sortable? [ctx]
  (let [?dbname (some-> (:hypercrud.browser/source-symbol ctx) str)
        ?last-segment (last (:hypercrud.browser/path ctx))]
    (and
      (if (and ?dbname (not (context/find-element-segment? ?last-segment))) ; [fe attr] or [attr], NOT [fe] or []
        (let [{:keys [:db/cardinality :db/valueType]} @(r/cursor (:hypercrud.browser/schemas ctx) [?dbname ?last-segment])]
          (and
            (= (:db/ident cardinality) :db.cardinality/one)
            ; ref requires more work (inspect label-prop)
            (contains? #{:db.type/keyword
                         :db.type/string
                         :db.type/boolean
                         :db.type/long
                         :db.type/bigint
                         :db.type/float
                         :db.type/double
                         :db.type/bigdec
                         :db.type/instant
                         :db.type/uuid
                         :db.type/uri
                         :db.type/bytes
                         :db.type/code}
                       (:db/ident valueType))))
        ; [fe] when aggregates or variables
        (and (not ?dbname) ?last-segment))
      @(r/track links-dont-break-sorting? ctx))))

(defn sort-direction [relative-path ctx]
  (let [[sort-path direction] @(::sort-col ctx)]
    (when (= sort-path relative-path)
      direction)))

(defn toggle-sort! [relative-path ctx]
  (when (sortable? ctx)
    (reset! (::sort-col ctx)
            (case (sort-direction relative-path ctx)
              :asc [relative-path :desc]
              :desc nil
              [relative-path :asc]))))
