(ns hyperfiddle.ui.sort
  (:require
    [hypercrud.browser.context :as context]
    [hyperfiddle.runtime :as runtime]))


(defn sortable? [ctx]
  (let [?dbname (context/dbname ctx)
        ?last-segment (last (:hypercrud.browser/path ctx))]
    ; Used to check links dont break sorting, but those cases don't happen anymore.
    (if (and ?dbname (context/attribute-segment? ?last-segment)) ; [fe attr] or [attr], NOT [fe] or []
      (let [{:keys [:db/cardinality :db/valueType]} @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas ?dbname ?last-segment])]
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
      (and (not ?dbname) ?last-segment))))

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
