(ns hyperfiddle.data
  (:require
    [cats.core :refer [mlet fmap]]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.link :as link]))


(defn relation-keyfn [relation]
  (hash (map #(or (:db/id %) %) relation)))

(defn form "Field is invoked as fn"
  [field {:keys [hypercrud.browser/ordered-fes] :as ctx}]
  (->> (r/unsequence ordered-fes)
       (mapcat (fn [[fe i]]
                 (concat
                   (->> fe
                        (r/fmap :fields)
                        (r/unsequence :attribute)
                        (mapv (fn [[_ a]]
                                (field [i a] ctx nil))))
                   (case (:type @fe)
                     :pull [(field [i] ctx nil)]            ; entity links
                     :aggregate nil
                     :variable nil                          ; We don't gen links for scalars, don't know if its a ref
                     ))))
       doall))

(defn browse' [rel path ctx]
  ; context is not set for this call
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
