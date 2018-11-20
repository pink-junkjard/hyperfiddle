(ns hyperfiddle.tempid
  (:require
    [contrib.datomic]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.field :as field]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.runtime :as runtime]))


(defn tempid? [id]
  (string? id))

(defn underlying-tempid [ctx id]
  ; This muddled thinking is caused by https://github.com/hyperfiddle/hyperfiddle/issues/584
  (cond
    (tempid? id) id
    :else (get (hypercrud.browser.context/ctx->id-lookup ctx) id)))

(defn smart-entity-identifier "Generates the best Datomic lookup ref for a given pull."
  [ctx {:keys [:db/id :db/ident] :as v}]                    ; v can be a ThinEntity or a pull i guess
  ; This must be called only on refs.
  ; If we have a color, and a (last path), ensure it is a ref.
  ; If we have a color and [] path, it is definitely a ref.
  ; If we have no color, it is a scalar or aggregate.
  ;(assert (::field/data-has-id? @(:hypercrud.browser/field ctx)) "smart-identity works only on refs")

  (let [identity-lookup nil]
    (or (if (underlying-tempid ctx id) id)                  ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
        ident
        identity-lookup
        id
        (if-not (map? v) v)                                 ; id-scalar
        nil                                                 ; This is an entity but you didn't pull any identity - error?
        )))

(defn stable-entity-key "Like smart-entity-identifier but reverses top layer of tempids to stabilize view keys in branches. You
  must pull db/id to trigger tempid detection! Don't use this in labels."
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Regression: Schema editor broken due to smart-id
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Form jank when tempid entity transitions to real entity
  (or (underlying-tempid ctx id)                            ; prefer the tempid for stability
      (smart-entity-identifier ctx v)))

(defn stable-relation-key "Stable key that works on scalars too. ctx is for tempid-reversing"
  [ctx v]
  (or (stable-entity-key ctx v) v))

(defn consistent-relation-key "Key that the dbval has, but unstable in views with a branch"
  [v]
  (or (contrib.datomic/smart-lookup-ref-no-tempids v) v))

(defn row-keyfn [ctx row]
  (r/row-keyfn' (partial stable-relation-key ctx) row))

(defn hash-data [ctx]                                       ; todo there are collisions when two links share the same 'location'
  (when-let [data (:hypercrud.browser/data ctx)]
    (case @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))
      :db.cardinality/one @(r/fmap->> data (stable-relation-key ctx))
      :db.cardinality/many @(r/fmap->> data
                                       (mapv (r/partial stable-relation-key ctx))
                                       (into #{})
                                       hash)                ; todo scalar
      nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant")))

(defn tempid-from-ctx "stable" [ctx]
  ; recurse all the way up the path? just data + parent-data is relative not fully qualified, which is not unique
  (-> (str (:hypercrud.browser/path ctx) "."
           (hash-data (:hypercrud.browser/parent ctx)) "."
           (hash-data ctx))
      hash str))

(defn tempid-from-stage "unstable"
  ([ctx]
   (let [dbname (context/dbname ctx)]
     (assert dbname "no dbname in dynamic scope (If it can't be inferred, write a custom formula)")
     (tempid-from-stage dbname ctx)))
  ([dbname ctx]
   @(r/fmap->> (runtime/state (:peer ctx) [::runtime/partitions])
               (branch/branch-val (context/uri dbname ctx) (:branch ctx))
               hash str)))

(defn ^:export with-tempid-color "tempids in hyperfiddle are colored, because we need the backing dbval in order to reverse hydrated
  dbid back into their tempid for routing"
  ([ctx factory]
   (let [dbname (context/dbname ctx)]
     (assert dbname "no dbname in dynamic scope (If it can't be inferred, write a custom formula)")
     (with-tempid-color dbname ctx factory)))
  ([dbname ctx factory]
   (->ThinEntity dbname (factory ctx))))
