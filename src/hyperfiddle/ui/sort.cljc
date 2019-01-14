(ns hyperfiddle.ui.sort
  (:require
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.browser.context :as context])
  #?(:clj
     (:import
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(defn sortable? [ctx]
  (let [element (:hypercrud.browser/element ctx)]
    ; Used to check links dont break sorting, but those cases don't happen anymore.
    (condp some [(type @element)]
      #{Aggregate Variable} true
      #{Pull} (let [[_ a _] @(:hypercrud.browser/eav ctx)]
                (and
                  (contrib.datomic/cardinality? @(:hypercrud.browser/schema ctx) a :db.cardinality/one)
                  ; ref requires more work (inspect label-prop)
                  (not= (:db.type/ref (contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a))))))))

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

(defn sort-fn [relations-val sort-col]
  (let [[path direction] @sort-col]
    (if path
      (sort-by #(get-in % path)
               (case direction
                 :asc #(compare %1 %2)
                 :desc #(compare %2 %1))
               relations-val)
      relations-val)))
