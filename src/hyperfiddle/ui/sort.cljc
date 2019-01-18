(ns hyperfiddle.ui.sort
  (:require
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.browser.context :as context])
  #?(:clj
     (:import
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull))))


(defn sortable? [ctx]
  (let [is-element-level (= (hypercrud.browser.context/pull-depth ctx) 0)
        element (some-> (:hypercrud.browser/element ctx) deref)] ; fiddle-attr level
    ; Used to check links dont break sorting, but those cases don't happen anymore.

    (if (and is-element-level element)                      ; fiddle-level but didn't focus an element
      (condp some [(type element)]
        #{Aggregate Variable} true
        #{Pull} (let [[_ a _] @(:hypercrud.browser/eav ctx)]
                  (and
                    (contrib.datomic/cardinality? @(:hypercrud.browser/schema ctx) a :db.cardinality/one)
                    ; ref requires more work (inspect label-prop)
                    (not= (:db.type/ref (contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a))))))
      ; e.g. !field[js/user.hyperblog-post-link]()
      false)))

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
