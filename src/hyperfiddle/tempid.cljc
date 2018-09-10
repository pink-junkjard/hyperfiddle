(ns hyperfiddle.tempid
  (:require
    [contrib.data :refer [abs-normalized]]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.runtime :as runtime]))


(defn smart-identity "Generates a Datomic lookup ref. Reverses one layer of tempids to avoid "
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Form jank when tempid entity transitions to real entity
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Regression: Schema editor broken due to smart-id
  (let [tempid (or (if (string? id) id)
                   (get (hypercrud.browser.context/ctx->id-lookup ctx) id))]
    (or tempid                                              ; the lookup ref is no good yet
        ident
        id
        (if-not (map? v) v)                                 ; id-scalar
        )))

(defn hash-data [ctx]                                       ; todo there are collisions when two links share the same 'location'
  (when-let [data (:hypercrud.browser/data ctx)]
    (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
      :db.cardinality/one @(r/fmap (r/partial smart-identity ctx) data)
      :db.cardinality/many (hash (into #{} @(r/fmap (r/partial mapv (r/partial smart-identity ctx)) data))) ; todo scalar
      nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))

(defn tempid-from-ctx "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  (-> (str (:hypercrud.browser/path ctx) "."
           (hash-data (:hypercrud.browser/parent ctx)) "."
           (hash-data ctx))
      hash abs-normalized - str))

(defn tempid-from-stage "unstable"
  ([ctx]
   (let [dbname (context/dbname ctx)]
     (assert dbname "no dbname in dynamic scope (If it can't be inferred, write a custom formula)")
     (tempid-from-stage dbname ctx)))
  ([dbname ctx]
   (-> @(r/fmap (r/partial branch/branch-val (context/uri dbname ctx) (:branch ctx))
                (runtime/state (:peer ctx) [::runtime/partitions]))
       hash abs-normalized - str)))

(defn ^:export with-tempid-color "tempids in hyperfiddle are colored, because we need the backing dbval in order to reverse hydrated
  dbid back into their tempid for routing"
  ([ctx factory]
   (let [dbname (context/dbname ctx)]
     (assert dbname "no dbname in dynamic scope (If it can't be inferred, write a custom formula)")
     (with-tempid-color dbname ctx factory)))
  ([dbname ctx factory]
   (->ThinEntity dbname (factory ctx))))
