(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.ui :refer [code-block code-inline-block edn-block edn-inline-block]]
    [contrib.ui.input :as input]
    [contrib.ui.recom-date :refer [recom-date]]

    ; This file is the hyperfiddle adapter for the controls, it has to generate
    ; datomic edits and knows about entity dbid. Perhaps the entity-attribute should be
    ; passed instead of the value?
    [hypercrud.browser.context :as context]))


(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(defn keyword [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))]
    [input/keyword-input* value on-change! props]))

(defn string [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    [input/input* value on-change! props]))

(defn long [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))]
    [input/validated-input
     value on-change!
     #(js/parseInt % 10) (fnil str "")
     #(or #_(= "" %) (integer? (js/parseInt % 10)))
     props]))

(defn ^:export boolean [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        change! #((:user-with! ctx) (tx/update-entity-attr @entity
                                                           @(:hypercrud.browser/fat-attribute ctx)
                                                           (not value)))]
    (contrib.ui/checkbox value change! props)))

(defn ^:export tristate-boolean [value ctx props]
  (let [entity (context/entity ctx)
        option-props {:disabled (or (cljs.core/boolean (:read-only props))
                                    (not @(r/fmap writable-entity? entity)))}
        value (if (nil? value) "" (str value))
        change! #(let [v (case (.-target.value %)
                           "" nil
                           "true" true
                           "false" false)]
                   ((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) v)))]
    [:select (-> props (merge {:value value :on-change change!}) (dissoc :label-fn))
     [:option (assoc option-props :key true :value "true") "True"]
     [:option (assoc option-props :key false :value "false") "False"]
     [:option (assoc option-props :key :nil :value "") "--"]]))

(defn dbid [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        on-change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))]
    [input/id-input value on-change! props]))

(defn ^:export instant [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))]
    [recom-date value change! props]))

(defn ^:export code [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        change! (fn [%]
                  (let [tx (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) (empty->nil %))]
                    ((:user-with! ctx) tx)))]
    (let [control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                    :hyperfiddle.ui.layout/block code-block
                    :hyperfiddle.ui.layout/table code-inline-block)]
      ; backwards args - props last
      [control props value change!])))

(defn ^:export markdown-editor [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) (empty->nil %)))]
    (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                   :hyperfiddle.ui.layout/block code-block
                   :hyperfiddle.ui.layout/table code-inline-block)
          props (assoc props :mode "markdown" :lineWrapping true)]
      ; backwards args - props last
      [widget props value change!])))

(defn ^:export edn-many [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
        value (set (if (= valueType :db.type/ref) (map :db/id value) value))
        change! (fn [user-val]
                  (let [user-val (set user-val)
                        rets (set/difference value user-val)
                        adds (set/difference user-val value)]
                    ((:user-with! ctx) (tx/edit-entity (:db/id @entity)
                                                       (:hypercrud.browser/attribute ctx)
                                                       rets adds))))
        widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/block edn-block
                 :hyperfiddle.ui.layout/table edn-inline-block)]
    [widget value change! props]))

(defn ^:export edn [value ctx props]
  (let [entity (context/entity ctx)
        props (update props :read-only #(or % (not @(r/fmap writable-entity? entity))))
        change! #((:user-with! ctx) (tx/update-entity-attr @entity @(:hypercrud.browser/fat-attribute ctx) %))
        widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/table edn-inline-block
                 :hyperfiddle.ui.layout/block edn-block)]
    [widget value change! props]))

(defn textarea* [{:keys [value on-change] :as props}]       ; unused
  (let [on-change #(let [newval (.. % -target -value)]
                     (on-change [value] [newval]))]
    [:textarea (assoc props :on-change on-change)]))
