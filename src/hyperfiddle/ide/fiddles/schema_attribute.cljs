(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.reagent :refer [from-react-context with-react-context]]
            [hypercrud.browser.context :as context]
            [hyperfiddle.data :as data]
            [hyperfiddle.ui :refer [field hyper-control markdown]]))


(def special-attrs #{:db/ident :db/cardinality :db/valueType})

(defn- completed? [entity] (set/subset? special-attrs (set (keys entity))))


; The rule is you can't stage anything until it's a valid Datomic attribute.
; So only the special attrs are editable at first.
; Once that is completed, the rest are editable.
(defn- read-only? [k record]
  (cond
    (special-attrs k) false
    :else (not (completed? record))))

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

(letfn [(user-with! [special-attrs-state ctx tx]
          (let [user-with! (:user-with! ctx)
                entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                new-entity (merge-in-tx entity tx ctx)]
            (case [(completed? entity) (completed? new-entity)]
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
  (defn- build-valueType-and-cardinality-renderer [special-attrs-state]
    (from-react-context
      (fn [{:keys [ctx props]} value]
        ; rebind with updated context
        [with-react-context {:ctx (assoc ctx :user-with! (r/partial user-with! special-attrs-state ctx))
                             :props props}
         [(hyper-control ctx) value]]))))

(letfn [(user-with!' [special-attrs-state ctx tx]
          (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                new-entity (merge-in-tx entity tx ctx)]
            (case [(completed? entity) (completed? new-entity)]
              [false false]
              ((:user-with! ctx) tx)

              [false true]
              (do
                ((:user-with! ctx) (tx/into-tx @special-attrs-state tx))
                (reset! special-attrs-state nil))

              [true false]
              ; todo this case WILL throw (going from a valid tx to invalid)
              ((:user-with! ctx) tx)

              [true true]
              ((:user-with! ctx) tx))))]
  (defn- build-ident-renderer [special-attrs-state]
    (from-react-context
      (fn [{:keys [ctx props]} value]
        ; rebind with updated context
        [with-react-context {:ctx (assoc ctx :user-with! (r/partial user-with!' special-attrs-state ctx))
                             :props props}
         [(hyper-control ctx) value]]))))

(defn renderer [ctx class]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx % @special-attrs-state ctx)
        controls {:db/cardinality (build-valueType-and-cardinality-renderer special-attrs-state)
                  :db/valueType (build-valueType-and-cardinality-renderer special-attrs-state)
                  :db/ident (build-ident-renderer special-attrs-state)}]
    (fn [ctx class]
      (let [ctx (-> ctx
                    (dissoc :hypercrud.browser/data :hypercrud.browser/data-cardinality :hypercrud.browser/path)
                    (update :hypercrud.browser/result (partial r/fmap reactive-merge))
                    (context/focus [:body]))
            result @(:hypercrud.browser/result ctx)]
        (into
          ^{:key (data/relation-keyfn @(:hypercrud.browser/data ctx))}
          [:div {:class class}
           [markdown "See [Datomic schema docs](https://docs.datomic.com/on-prem/schema.html)."]
           (let [k :db/ident] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           (let [k :db/valueType] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           (let [k :db/cardinality] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           (let [k :db/doc] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           (let [k :db/unique] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           [markdown "!block[Careful: below is not validated, don't stage invalid schema]{.alert .alert-warning style=\"margin-bottom: 0\"}"]
           (let [k :db/isComponent] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           (let [k :db/fulltext] (field [0 k] ctx (controls k) {:read-only (read-only? k result)}))
           ])))))
