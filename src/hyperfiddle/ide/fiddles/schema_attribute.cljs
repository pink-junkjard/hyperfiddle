(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [hypercrud.browser.context :as context]
            [hyperfiddle.ui :refer [field hyper-control]]))


(def special-case-attrs #{:db/ident :db/cardinality :db/valueType})

(defn- has-required-attrs? [entity] (set/subset? special-case-attrs (set (keys entity))))

(defn- read-only? [attr record]
  (not (or (has-required-attrs? record)
           (#{:db/ident :db/doc :db/valueType :db/cardinality} attr))))

(defn- merge-in-tx [entity tx ctx]
  (reduce (fn [entity [op e a v]]
            ; todo this fn has bare minimum support for this page
            ; e.g. doesnt support card/many or nested modals
            (let [valueType @(r/cursor (:hypercrud.browser/schemas ctx) ["$" a :db/valueType :db/ident])
                  v (if (= :db.type/ref valueType)
                      {:db/id v}
                      v)]
              (case op
                :db/add (assoc entity a v)
                :db/retract (dissoc entity a))))
          (into {} entity)
          tx))

(defn- build-valueType-and-cardinality-renderer [special-attrs-state]
  (let [user-with! (fn [ctx user-with! tx]
                     (let [entity @(:cell-data ctx)
                           new-entity (merge-in-tx entity tx ctx)]
                       (case [(has-required-attrs? entity) (has-required-attrs? new-entity)]
                         [false false]
                         (swap! special-attrs-state tx/into-tx tx)

                         [false true]
                         (do
                           (user-with! (tx/into-tx @special-attrs-state tx))
                           (reset! special-attrs-state nil))

                         [true false]
                         ; todo this case WILL throw (going from a valid tx to invalid)
                         (user-with! tx)

                         [true true]
                         (user-with! tx))))]
    (fn [value ctx props]
      [(hyper-control ctx)
       value
       (update ctx :user-with! #(r/partial user-with! ctx %))
       props])))

(defn- build-ident-renderer [special-attrs-state]
  (let [user-with! (fn [ctx user-with! tx]
                     (let [entity @(:cell-data ctx)
                           new-entity (merge-in-tx entity tx ctx)]
                       (case [(has-required-attrs? entity) (has-required-attrs? new-entity)]
                         [false false]
                         (user-with! tx)

                         [false true]
                         (do
                           (user-with! (tx/into-tx @special-attrs-state tx))
                           (reset! special-attrs-state nil))

                         [true false]
                         ; todo this case WILL throw (going from a valid tx to invalid)
                         (user-with! tx)

                         [true true]
                         (user-with! tx))))]
    (fn [value ctx props]
      [(hyper-control ctx)
       value
       (update ctx :user-with! #(r/partial user-with! ctx %))
       props])))

(declare renderer)

(def attrs [:db/ident :db/valueType :db/cardinality :db/doc
            :db/unique :db/isComponent :db/fulltext])

(defn renderer [ctx]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx % @special-attrs-state ctx)
        controls {:db/cardinality (build-valueType-and-cardinality-renderer special-attrs-state)
                  :db/valueType (build-valueType-and-cardinality-renderer special-attrs-state)
                  :db/ident (build-ident-renderer special-attrs-state)}]
    (fn [ctx]
      (let [ctx (-> ctx
                    (dissoc :relation :relations)
                    (update :hypercrud.browser/result (partial r/fmap reactive-merge))
                    (context/with-relations))
            result @(:hypercrud.browser/result ctx)]
        (into
          [:div]
          (for [k attrs]
            (let [ro (read-only? k result)]
              (field [0 k] ctx (controls k) :read-only ro))))))))
