(ns hyperfiddle.tempid
  (:require
    [contrib.data :refer [abs-normalized]]
    [contrib.reactive :as r]
    [hypercrud.browser.field :as field]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]))


(defn hash-data [ctx]                                       ; todo there are collisions when two links share the same 'location'
  (when-let [data (:hypercrud.browser/data ctx)]
    (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
      :db.cardinality/one @(r/fmap :db/id data)
      :db.cardinality/many (hash (into #{} @(r/fmap (partial mapv :db/id) data))) ; todo scalar
      nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))

(defn tempid-from-ctx "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  (-> (str (:hypercrud.browser/path ctx) "."
           (hash-data (:hypercrud.browser/parent ctx)) "."
           (hash-data ctx))
      hash abs-normalized - str))

(defn tempid-from-stage "unstable" [ctx]
  (-> @(r/fmap (r/partial branch/branch-val (:uri ctx) (:branch ctx)) (runtime/state (:peer ctx) [::runtime/partitions]))
      hash abs-normalized - str))

(defn ^:export with-tempid-color "tempids in hyperfiddle are colored, because we need the backing dbval in order to reverse hydrated
  dbid back into their tempid for routing"
  [ctx factory]
  (assert (:uri ctx) "no uri in dynamic scope (If it can't be inferred, write a custom formula)")
  (->ThinEntity (domain/uri->dbname (:uri ctx) (:hypercrud.browser/domain ctx)) (factory ctx)))
