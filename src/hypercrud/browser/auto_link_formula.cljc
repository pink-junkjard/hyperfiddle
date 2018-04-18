(ns hypercrud.browser.auto-link-formula                     ; namespace is public export runtime
  #?(:cljs (:require-macros [hypercrud.browser.auto-link-formula :refer [build-auto-formula-lookup]]))
  (:require [cats.monad.either :as either]
            [contrib.data :refer [abs-normalized map-keys]]
            [contrib.reactive :as r]
            [contrib.string :refer [memoized-safe-read-edn-string]]
            [contrib.vedn :as vedn]
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

#?(:clj
   (defmacro build-auto-formula-lookup []
     (let [fe-no-create (macroexpand `(vedn/load-vedn-from-file "auto-formula/fe-no-create.vedn"))
           fe-create (macroexpand `(vedn/load-vedn-from-file "auto-formula/fe-create.vedn"))
           ; no fe = index or relation links
           no-fe {{:fe false :c? false :d? true :a false} nil
                  {:fe false :c? false :d? true :a true} nil
                  {:fe false :c? false :d? false :a false} (get fe-no-create {:d? false :a false})
                  {:fe false :c? false :d? false :a true} nil

                  {:fe false :c? true :d? true :a false} nil
                  {:fe false :c? true :d? true :a true} nil
                  {:fe false :c? true :d? false :a false} (get fe-create {:d? false :a false})
                  {:fe false :c? true :d? false :a true} nil}]
       (merge (map-keys #(assoc % :fe true :c? true) fe-create)
              (map-keys #(assoc % :fe true :c? false) fe-no-create)
              no-fe))))

(def auto-formula-lookup (build-auto-formula-lookup))

(defn auto-formula [link]
  (-> (memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
      (either/branch
        (fn [e]
          (timbre/error e)
          nil)
        (fn [path]
          (get auto-formula-lookup
               {:fe (not (nil? (first path)))
                :c? (or (:link/create? link) false)
                :d? (or (:link/dependent? link) false)
                :a (not (nil? (second path)))})))))
