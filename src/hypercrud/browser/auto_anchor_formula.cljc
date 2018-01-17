(ns hypercrud.browser.auto-anchor-formula                   ; namespace is public export runtime
  (:require [cats.monad.either :as either]
            [hypercrud.browser.dbname :as dbname]
            [hypercrud.types.Entity :refer [#?(:cljs Entity)]]
            [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
            [hypercrud.util.core :as util]
            [hypercrud.util.string :as hc-string]
            [hypercrud.util.vedn :as vedn]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.Entity Entity)
              (hypercrud.types.ThinEntity ThinEntity))))


(defn auto-entity-from-stage [ctx]
  ; This returns a new value each time the transaction changes - can't call it again later.
  ; So tx-fns must inspect the modal-route, they can't re-create the dbid.
  (let [id (-> (:branch ctx) util/abs-normalized - str)]
    (->ThinEntity (dbname/uri->dbname (:uri ctx) ctx) id)))

; todo there are collisions when two links share the same 'location'
(defn deterministic-ident
  ([ctx]
   (deterministic-ident
     (:find-element ctx)
     (:cell-data ctx)
     (:attribute ctx)
     (:value ctx)))
  ([fe cell-data a v]
    ; Need comment explaining why.
    ; [fe e a v] quad is sufficient to answer "where are we".
    ; Why Db is omitted?
    ; Why value is only inspected in :many for unique hashing?
   (-> (str (:name fe) "."
            (or (:db/id (some-> cell-data deref)) (hash (some-> cell-data deref))) "."
            (-> a :db/ident) "."
            (case (get-in a [:db/cardinality :db/ident])
              :db.cardinality/one nil
              :db.cardinality/many (hash (into #{} (mapv :db/id @v))) ; todo scalar
              nil nil #_":db/id has a faked attribute with no cardinality, need more thought to make elegant"))
       hash util/abs-normalized - str)))

(defn auto-entity [ctx]
  (let [cell-data @(:cell-data ctx)
        uri (if (instance? Entity cell-data)
              (.-uri cell-data)
              (:uri ctx))]
    (->ThinEntity (dbname/uri->dbname uri ctx) (deterministic-ident ctx))))

(def auto-formula-lookup
  (let [fe-no-create (->> (vedn/load-vedn-from-file "auto-formula/fe-no-create.vedn")
                          (util/map-keys #(assoc % :fe true :c? false)))
        fe-create (->> (vedn/load-vedn-from-file "auto-formula/fe-create.vedn")
                       (util/map-keys #(assoc % :fe true :c? true)))
        ; no fe = index or relation links
        no-fe {{:fe false :c? false :d? true :a false} nil
               {:fe false :c? false :d? true :a true} nil
               {:fe false :c? false :d? false :a false} nil
               {:fe false :c? false :d? false :a true} nil

               {:fe false :c? true :d? true :a false} nil
               {:fe false :c? true :d? true :a true} nil
               {:fe false :c? true :d? false :a false} nil
               {:fe false :c? true :d? false :a true} nil}]
    (merge fe-create fe-no-create no-fe)))

(defn auto-formula [link]
  (-> (hc-string/memoized-safe-read-edn-string (str "[" (:link/path link) "]"))
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
