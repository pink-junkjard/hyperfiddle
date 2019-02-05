(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require
    [clojure.set :as set]
    [contrib.reactive :as r]
    [contrib.datomic-tx :as tx]
    [contrib.ui :refer [debounced]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [field markdown]]
    [hyperfiddle.ui.util :refer [entity-change->tx with-tx!]]))


(def special-attrs #{:db/ident :db/cardinality :db/valueType})

(defn- completed? [entity] (set/subset? special-attrs (set (keys entity))))

(defn- merge-in-tx [entity tx ctx]
  ; this fn has bare minimum support for this page e.g. doesnt support card/many or nested modals
  (reduce (fn [entity [op e a v]]
            (case op
              :db/add (assoc entity a v)                    ; these are lookup refs because of the options query
              :db/retract (dissoc entity a)))
          entity
          tx))

(defn valueType-and-cardinality-with-tx! [special-attrs-state ctx tx]
  (let [entity (hypercrud.browser.context/data ctx)
        uri @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/ident name
                       (context/uri ctx))
        schema @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas uri])
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (swap! special-attrs-state (partial tx/into-tx schema) tx)

      [false true]
      (do
        (with-tx! ctx (tx/into-tx schema @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (with-tx! ctx tx)

      [true true]
      (with-tx! ctx tx))))

(defn ident-with-tx! [special-attrs-state ctx tx]
  (let [entity (hypercrud.browser.context/data ctx)
        uri @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/ident name
                       (context/uri ctx))
        schema @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas uri])
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (with-tx! ctx tx)

      [false true]
      (do
        (with-tx! ctx (tx/into-tx schema @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (with-tx! ctx tx)

      [true true]
      (with-tx! ctx tx))))

(defn renderer [val ctx props]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx @% @special-attrs-state ctx)
        ident-f (fn [val ctx props]
                  (let [on-change! (r/comp (r/partial ident-with-tx! special-attrs-state ctx)
                                           (r/partial entity-change->tx ctx))
                        props (assoc props :value @(:hypercrud.browser/result ctx)
                                           :on-change on-change!)]
                    [debounced props contrib.ui/keyword]))
        valueType-and-cardinality-f (fn [val ctx props]
                                      (let [on-change! (r/comp (r/partial valueType-and-cardinality-with-tx! special-attrs-state ctx)
                                                               (r/partial entity-change->tx ctx))]
                                        [hyperfiddle.ui/hyper-control val ctx (assoc props :on-change on-change!)]))]
    (fn [val ctx props]
      (let [ctx (update ctx :hypercrud.browser/result (partial r/track reactive-merge))
            valid-attr? @(r/fmap completed? (:hypercrud.browser/result ctx))]
        [:div props
         [markdown "See [Datomic schema docs](https://docs.datomic.com/on-prem/schema.html)."]
         (field [:db/ident] ctx ident-f)
         (field [:db/valueType] ctx valueType-and-cardinality-f {:options "valueType-options"})
         (field [:db/cardinality] ctx valueType-and-cardinality-f {:options "cardinality-options"})

         ; The rule is you can't stage anything until it's a valid Datomic attribute.
         ; So only the special attrs are editable at first.
         ; Once that is completed, the rest are editable.
         (field [:db/doc] ctx nil {:disabled (not valid-attr?)})
         (field [:db/unique] ctx hyperfiddle.ui/hyper-control {:disabled (not valid-attr?)
                                                               :options "unique-options"})
         [markdown "!block[Careful: below is not validated, don't stage invalid schema]{.alert .alert-warning style=\"margin-bottom: 0\"}"]
         (field [:db/isComponent] ctx nil {:disabled (not valid-attr?)})
         (field [:db/fulltext] ctx nil {:disabled (not valid-attr?)})
         ]))))
