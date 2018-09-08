(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.ui :refer [debounced]]
            [hypercrud.browser.context :as context]
            [hyperfiddle.ui :refer [field markdown]]
            [hyperfiddle.ui.util :refer [entity-props readonly->disabled on-change->tx writable-entity? entity-change->tx]]))


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

(defn renderer [val ctx props]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx % @special-attrs-state ctx)
        ident-f (fn [val ctx props]
                  (let [on-change! (r/comp (r/partial ident-with-tx! special-attrs-state ctx)
                                           (r/partial on-change->tx ctx))
                        props (-> (assoc props :value @(:hypercrud.browser/data ctx)
                                               :on-change on-change!)
                                  readonly->disabled)]
                    [debounced props contrib.ui/keyword]))
        valueType-and-cardinality-f (fn [val ctx props]
                                      (let [on-change! (r/comp (r/partial valueType-and-cardinality-with-tx! special-attrs-state ctx)
                                                               (r/partial entity-change->tx ctx))]
                                        [hyperfiddle.ui/hyper-control val ctx (assoc props :on-change on-change!)]))]
    (fn [val ctx props]
      (let [ctx (update ctx :hypercrud.browser/data (partial r/fmap reactive-merge))
            valid-attr? @(r/fmap completed? (:hypercrud.browser/data ctx))]
        [:div props
         [markdown "See [Datomic schema docs](https://docs.datomic.com/on-prem/schema.html)."]
         (field [:db/ident] ctx ident-f)
         (field [:db/valueType] ctx valueType-and-cardinality-f {:options "valueType-options"})
         (field [:db/cardinality] ctx valueType-and-cardinality-f {:options "cardinality-options"})

         ; The rule is you can't stage anything until it's a valid Datomic attribute.
         ; So only the special attrs are editable at first.
         ; Once that is completed, the rest are editable.
         (field [:db/doc] ctx nil {:read-only (not valid-attr?)})
         (field [:db/unique] ctx hyperfiddle.ui/hyper-control {:read-only (not valid-attr?)
                                                               :options "unique-options"})
         [markdown "!block[Careful: below is not validated, don't stage invalid schema]{.alert .alert-warning style=\"margin-bottom: 0\"}"]
         (field [:db/isComponent] ctx nil {:read-only (not valid-attr?)})
         (field [:db/fulltext] ctx nil {:read-only (not valid-attr?)})
         ]))))
