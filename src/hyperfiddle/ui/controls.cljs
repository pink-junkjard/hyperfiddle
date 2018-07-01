(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.reagent :refer [from-react-context]]
    [contrib.string :refer [empty->nil]]
    [contrib.ui :refer [code-block code-inline-block edn-block edn-inline-block]]
    [contrib.ui.input :as input]
    [contrib.ui.recom-date :refer [recom-date]]

    ; This file is the hyperfiddle adapter for the controls, it has to generate
    ; datomic edits and knows about entity dbid. Perhaps the entity-attribute should be
    ; passed instead of the value?
    [hypercrud.browser.context :as context]))


(defn entity-change! [ctx value]
  ; Sometimes value is partialed, sometimes not, depends on the widget's change! interface which is inconsistent
  (let [value (empty->nil value) #_"safe for non-strings"]
    ((:user-with! ctx) (tx/update-entity-attr @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                                              @(:hypercrud.browser/fat-attribute ctx)
                                              value))))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(def ^:export keyword
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
            on-change! (r/partial entity-change! ctx)]
        [input/keyword-input* value on-change! props]))))

(def ^:export string
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
            on-change! (r/partial entity-change! ctx)]
        [input/input* value on-change! props]))))

(def ^:export long
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
            on-change! (r/partial entity-change! ctx)]
        [input/validated-input value on-change!
         #(js/parseInt % 10) (fnil str "")
         #(or #_(= "" %) (integer? (js/parseInt % 10)))
         props]))))

(def ^:export boolean
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [contrib.ui/easy-checkbox "" value
       (r/partial entity-change! ctx (not value))
       (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))

(def ^:export tristate-boolean
  (letfn [(adapter [e]
            (case (.-target.value e)
              "" nil
              "true" true
              "false" false))
          (change! [ctx v]
            (entity-change! ctx (adapter v)))]
    (from-react-context
      (fn [{:keys [ctx props]} value]
        (let [option-props {:disabled (or (cljs.core/boolean (:read-only props))
                                          (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))}]
          [:select (-> (dissoc props :label-fn)
                       (merge {:value (if (nil? value) "" (str value))
                               :on-change (r/partial change! ctx)}))
           [:option (assoc option-props :key true :value "true") "True"]
           [:option (assoc option-props :key false :value "false") "False"]
           [:option (assoc option-props :key :nil :value "") "--"]])))))

(def ^:export dbid
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [input/id-input value
       (r/partial entity-change! ctx)
       (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))

(def ^:export instant
  (from-react-context
    (fn [{:keys [ctx props]} value]
      [recom-date value
       (r/partial entity-change! ctx)
       (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))

(def ^:export code
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                      :hyperfiddle.ui.layout/block code-block
                      :hyperfiddle.ui.layout/table code-inline-block)]
        ; code has backwards args - props first
        [control
         (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
         value
         (r/partial entity-change! ctx)]))))

(def ^:export markdown-editor
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                     :hyperfiddle.ui.layout/block code-block
                     :hyperfiddle.ui.layout/table code-inline-block)]
        ;code has backwards args - props first
        [widget
         (-> props
             (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
             (assoc :mode "markdown" :lineWrapping true))
         value
         (r/partial entity-change! ctx)]))))

(def ^:export edn-many
  (letfn [(change! [ctx value user-val]
            (let [user-val (set user-val)
                  rets (set/difference value user-val)
                  adds (set/difference user-val value)]
              ((:user-with! ctx) (tx/edit-entity @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
                                                 (:hypercrud.browser/attribute ctx)
                                                 rets adds))))]
    (from-react-context
      (fn [{:keys [ctx props]} value]
        (let [valueType @(r/cursor (:hypercrud.browser/fat-attribute ctx) [:db/valueType :db/ident])
              value (set (if (= valueType :db.type/ref) (map :db/id value) value))
              widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                       :hyperfiddle.ui.layout/block edn-block
                       :hyperfiddle.ui.layout/table edn-inline-block)]
          [widget value
           (r/partial change! ctx value)
           (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))))

(def ^:export edn
  (from-react-context
    (fn [{:keys [ctx props]} value]
      (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                     :hyperfiddle.ui.layout/table edn-inline-block
                     :hyperfiddle.ui.layout/block edn-block)]
        [widget value
         (r/partial entity-change! ctx)
         (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]))))

;(defn textarea* [{:keys [value on-change] :as props}]       ; unused
;  (let [on-change #(let [newval (.. % -target -value)]
;                     (on-change [value] [newval]))]
;    [:textarea (assoc props :on-change on-change)]))
