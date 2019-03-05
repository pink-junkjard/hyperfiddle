(ns hyperfiddle.ui.sort
  (:require
    [contrib.data :refer [unqualify]]
    [contrib.datomic]
    [hypercrud.browser.context :as context]))


(defn sortable? [ctx]
  (let [element (some-> (:hypercrud.browser/element ctx) deref)]
    ; Used to check links dont break sorting, but those cases don't happen anymore.
    (if (and element (not (context/qfind-level? ctx)))
      (condp some [(unqualify (contrib.datomic/parser-type element))]
        #{:aggregate :variable} true
        #{:pull} (let [a (context/a ctx)]
                  (and
                    (context/attr? ctx a :db.cardinality/one)
                    ; ref requires more work (inspect label-prop)
                    (not (context/attr? ctx a :db.type/ref)))))
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
