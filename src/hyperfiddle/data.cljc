(ns hyperfiddle.data
  (:require
    [cats.core :refer [mlet fmap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.link :as link]))


(defn relation-keyfn [relation]
  (hash (map #(or (:db/id %) %) relation)))

(defn form [field {:keys [hypercrud.browser/ordered-fes] :as ctx}]
  ; Want element and naked links towards the right, so use vectors for conj-end
  (-> ordered-fes
      (r/unsequence)
      (->> (mapcat (fn [[fe i]]
                     (conj
                       (->> fe
                            (r/fmap :fields)
                            (r/unsequence :attribute)
                            (mapv (fn [[_ a]]
                                    (field [i a] ctx nil))))
                       (field [i] ctx nil))))
           vec)
      (conj (field [] ctx nil))
      seq                                                   ; This has to seq on the way out or it will be treated as Hiccup.
      doall))

(defn browse' [rel path ctx]
  (->> (base/data-from-link @(r/track link/rel->link rel path ctx) ctx)
       (fmap :hypercrud.browser/result)
       (fmap deref)))

(defn attr-sortable? [fe attribute ctx]
  (if-let [dbname (some-> (:source-symbol fe) str)]
    (let [{:keys [:db/cardinality :db/valueType]} @(r/cursor (:hypercrud.browser/schemas ctx) [dbname attribute])]
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
    (not (nil? fe))))

(defn sort-fn [sort-col ctx relations-val]
  (let [[sort-fe-pos sort-attr direction] @sort-col
        fe @(r/cursor (:hypercrud.browser/ordered-fes ctx) [sort-fe-pos])
        attr (->> (map :attribute (:fields fe))
                  (filter #(= % sort-attr))
                  first)]
    (if (attr-sortable? fe attr ctx)
      (let [sort-fn (if sort-attr
                      #(get-in % [sort-fe-pos sort-attr])
                      #(get % sort-fe-pos))]
        (sort-by sort-fn
                 (case direction
                   :asc #(compare %1 %2)
                   :desc #(compare %2 %1))
                 relations-val))
      relations-val)))
