(ns hypercrud.browser.auto-link-formula                     ; namespace is public export runtime
  (:require [cats.monad.either :as either]
            [contrib.data :refer [abs-normalized]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [hypercrud.browser.dbname :as dbname]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [hypercrud.util.branch :as branch]
            [hyperfiddle.runtime :as runtime]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Entity Entity))))


(defn ^:export auto-entity-from-stage [ctx]
  ; This returns a new value each time the transaction changes - can't call it again later.
  ; So tx-fns must inspect the modal-route, they can't re-create the dbid.
  (assert (:uri ctx) "no uri in dynamic scope (If it can't be inferred, add as bindings)")
  (let [branch-val (branch/branch-val (:uri ctx) (:branch ctx) @(runtime/state (:peer ctx) [:stage]))
        id (-> branch-val hash abs-normalized - str)]
    (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) id)))

; todo there are collisions when two links share the same 'location'
(defn deterministic-ident
  ([ctx]
   (deterministic-ident
     (:hypercrud.browser/find-element ctx)
     (:cell-data ctx)
     (:hypercrud.browser/fat-attribute ctx)
     (:value ctx)))
  ([fe cell-data a v]
    ; Need comment explaining why.
    ; [fe e a v] quad is sufficient to answer "where are we".
    ; Why Db is omitted?
    ; Why value is only inspected in :many for unique hashing?
   (-> (str (some-> fe (r/cursor [:name]) deref) "."
            (or (some-> cell-data (r/cursor [:db/id]) deref)
                (hash (some-> cell-data deref))) "."
            (some-> a (r/cursor [:db/ident]) deref) "."
            (case (some-> a (r/cursor [:db/cardinality :db/ident]) deref)
              :db.cardinality/one nil
              :db.cardinality/many (hash (into #{} (mapv :db/id @v))) ; todo scalar
              nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant"))
       hash abs-normalized - str)))

(defn auto-entity [ctx]
  (let [cell-data @(:cell-data ctx)
        uri (if (instance? Entity cell-data)
              (.-uri cell-data)
              (:uri ctx))]
    (->ThinEntity (dbname/uri->dbname uri ctx) (deterministic-ident ctx))))

(defn -auto-formula-impl [ctx path & {:keys [create? dependent?]}]
  (case {:fe (not (nil? (first path))) :c? (or create? false) :d? (or dependent? false)}
    {:fe true :c? false :d? true} (if (nil? (second path))
                                    (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                                      "(comp deref :cell-data)")
                                    (let [dbname (str @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :source-symbol]))]
                                      (case @(r/cursor (:hypercrud.browser/schemas ctx) [dbname (second path) :db/cardinality :db/ident])
                                        :db.cardinality/one "(comp deref :value)"

                                        ; "find children of parent entity at attr". See base ->EntityRequest
                                        :db.cardinality/many (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                                                               "(juxt (comp deref :cell-data) :hypercrud.browser/attribute)")

                                        ; the attribute doesnt exist
                                        nil "(comp deref :value)")))
    {:fe true :c? false :d? false} nil
    {:fe true :c? true :d? true} (when @(r/cursor (:hypercrud.browser/ordered-fes ctx) [(first path) :entity])
                                   "hypercrud.browser.auto-link-formula/auto-entity")
    {:fe true :c? true :d? false} "hypercrud.browser.auto-link-formula/auto-entity-from-stage"

    ; no fe = index or relation links
    {:fe false :c? false :d? true} nil
    {:fe false :c? false :d? false} nil
    {:fe false :c? true :d? true} nil
    {:fe false :c? true :d? false} (when (nil? (second path))
                                     "hypercrud.browser.auto-link-formula/auto-entity-from-stage")))

(defn auto-formula [ctx link]
  (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (either/branch
        (fn [e]
          (timbre/error e)
          nil)
        (fn [path]
          (-auto-formula-impl ctx path :create? (:link/create? link) :dependent? (:link/dependent? link))))))
