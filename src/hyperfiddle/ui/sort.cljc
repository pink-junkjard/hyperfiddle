(ns hyperfiddle.ui.sort
  (:require
    [contrib.data :refer [unqualify]]
    [contrib.datomic]
    [hypercrud.browser.context :as context]))


(defn top-level-sort? [ctx]
  ; Special if statement because our context doesn't track this weird case.
  ; result-path assumes there is a row in scope but we are in the header.
  ; pull-path does not track find elements.
  ;
  ; Top level header, we might be in a FE
  (or (= 1 (context/pull-depth ctx))
      (and (context/qfind-level? ctx)                       ; no pull path, e.g. aggregate
           (boolean (:hypercrud.browser/element ctx)))))

(defn sortable? [ctx]
  ; check attr, not a, because context/a returns fiddle-ident but attr checks the schema

  (or (and (context/attr ctx)
           (context/attr? ctx :db.cardinality/one)
           (not (context/attr? ctx :db.type/ref)))
      (and (boolean (:hypercrud.browser/element ctx))
           (not (context/attr ctx)))))

(defn sort-direction [p ctx]
  (let [[sort-path direction] @(::sort-col ctx)]
    (when (= sort-path p)
      direction)))

(defn sort-path [ctx]
  (when (sortable? ctx)                                     ; we are inside a table header
    (cond
      (top-level-sort? ctx)                                 ; bypasses aggregate?
      (condp = [(unqualify (contrib.datomic/parser-type (context/qfind ctx)))
                (unqualify (contrib.datomic/parser-type (context/element ctx)))]
        [:find-coll :pull] [(context/a ctx)]
        [:find-rel :pull] [(:hypercrud.browser/element-index ctx) (context/a ctx)]
        [:find-coll :aggregate] nil
        [:find-rel :aggregate] [(:hypercrud.browser/element-index ctx)]

        ; Scalar and tuple aren't sortable
        nil)

      :else-nested-pull                                     ; Relative to the collection (:many value).
      [(context/a ctx)])))

(defn sort-directive [ctx]
  (if-let [p (sort-path ctx)]
    [p (sort-direction p ctx)]))

(defn toggle-sort! [ctx]
  (if-let [[p ?d] (sort-directive ctx)]
    (let [v (case ?d
              :asc [p :desc]
              :desc nil                                     ; [ nil nil ]
              [p :asc])]
      (reset! (::sort-col ctx) v))))

(defn sort-fn [relations-val sort-col]
  (let [[path direction] @sort-col]
    (if path
      (sort-by #(get-in % path)
               (case direction
                 :asc #(compare %1 %2)
                 :desc #(compare %2 %1))
               relations-val)
      relations-val)))
