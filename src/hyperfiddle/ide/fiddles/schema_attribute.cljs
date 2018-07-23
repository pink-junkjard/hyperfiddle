(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui.input :as input]
            [hypercrud.browser.context :as context]
            [hyperfiddle.data :as data]
            [hyperfiddle.ui :refer [field markdown]]
            [hyperfiddle.ui.controls :as controls]
            [hyperfiddle.ui.select :refer [select]]))


(def special-attrs #{:db/ident :db/cardinality :db/valueType})

(defn- completed? [entity] (set/subset? special-attrs (set (keys entity))))

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

(defn valueType-and-cardinality-with-tx! [special-attrs-state ctx tx]
  (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (swap! special-attrs-state tx/into-tx tx)

      [false true]
      (do
        (context/with-tx! ctx (tx/into-tx @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (context/with-tx! ctx tx)

      [true true]
      (context/with-tx! ctx tx))))

(defn ident-with-tx! [special-attrs-state ctx tx]
  (let [entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (context/with-tx! ctx tx)

      [false true]
      (do
        (context/with-tx! ctx (tx/into-tx @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (context/with-tx! ctx tx)

      [true true]
      (context/with-tx! ctx tx))))

(defn renderer [ctx class]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx % @special-attrs-state ctx)
        ident-f (fn [ref props ctx]
                  (let [on-change! (r/comp (r/partial ident-with-tx! special-attrs-state ctx)
                                           (r/partial controls/entity-change->tx ctx))]
                    [input/keyword-input* @(:hypercrud.browser/data ctx) on-change! props]))
        valueType-and-cardinality-f (fn [ref props ctx]
                                      (let [on-change! (r/comp (r/partial valueType-and-cardinality-with-tx! special-attrs-state ctx)
                                                               (r/partial controls/entity-change->tx ctx))]
                                        [select (assoc props :on-change on-change!) ctx]))]
    (fn [ctx class]
      (let [ctx (-> ctx
                    (dissoc :hypercrud.browser/data :hypercrud.browser/data-cardinality :hypercrud.browser/path)
                    (update :hypercrud.browser/result (partial r/fmap reactive-merge))
                    (context/focus [:body]))
            valid-attr? @(r/fmap completed? (:hypercrud.browser/result ctx))]
        ^{:key (data/relation-keyfn @(:hypercrud.browser/data ctx))}
        [:div {:class class}
         [markdown "See [Datomic schema docs](https://docs.datomic.com/on-prem/schema.html)."]
         (field [0 :db/ident] ctx ident-f)
         (field [0 :db/valueType] ctx valueType-and-cardinality-f)
         (field [0 :db/cardinality] ctx valueType-and-cardinality-f)

         ; The rule is you can't stage anything until it's a valid Datomic attribute.
         ; So only the special attrs are editable at first.
         ; Once that is completed, the rest are editable.
         (field [0 :db/doc] ctx nil {:read-only valid-attr?})
         (field [0 :db/unique] ctx nil {:read-only valid-attr?})
         [markdown "!block[Careful: below is not validated, don't stage invalid schema]{.alert .alert-warning style=\"margin-bottom: 0\"}"]
         (field [0 :db/isComponent] ctx nil {:read-only valid-attr?})
         (field [0 :db/fulltext] ctx nil {:read-only valid-attr?})
         ]))))
