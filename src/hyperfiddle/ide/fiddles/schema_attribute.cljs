(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require [clojure.set :as set]
            [contrib.datomic-tx :as tx]
            [contrib.reactive :as r]
            [contrib.reagent :refer [from-react-context fix-arity-1-with-context]]
            [hypercrud.browser.context :as context]
            [hyperfiddle.ui :refer [field hyper-control]]))


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
        [fix-arity-1-with-context                           ; rebind with updated context
         (hyper-control ctx)
         value
         (assoc ctx :user-with! (r/partial user-with! special-attrs-state ctx))
         props]))))

(letfn [(user-with!' [special-attrs-state ctx tx]
          (let [entity @(context/entity ctx)
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
        [fix-arity-1-with-context                           ; rebind with updated context
         (hyper-control ctx)
         value
         (assoc ctx :user-with! (r/partial user-with!' special-attrs-state ctx))
         props]))))

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
                    (dissoc :hypercrud.browser/data :hypercrud.browser/data-cardinality :hypercrud.browser/path)
                    (update :hypercrud.browser/result (partial r/fmap reactive-merge))
                    (context/focus [:body]))
            result @(:hypercrud.browser/result ctx)]
        (into
          [:div]
          (for [k attrs]
            (let [ro (read-only? k result)]
              (field [0 k] ctx (controls k) {:read-only ro}))))))))
