(ns hyperfiddle.ui.controls
  (:refer-clojure :exclude [boolean keyword long])
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [contrib.ui]                                            ; avoid collisions
    [contrib.ui.input :as input :refer [adapt-props]]
    [contrib.ui.recom-date :refer [recom-date]]
    [hypercrud.browser.context :as context]

    ; This file is the hyperfiddle adapter for the controls, it has to generate
    ; datomic edits and knows about entity dbid. Perhaps the entity-attribute should be
    ; passed instead of the value?
    ))


(defn entity-change->tx [ctx value]
  (let [value (empty->nil value)                            ; safe for non-strings
        entity @(get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
        attribute @(context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx))]
    (tx/update-entity-attr entity attribute value)))

(defn entity-change! [ctx value]
  ; Sometimes value is partialed, sometimes not, depends on the widget's change! interface which is inconsistent
  (context/with-tx! ctx (entity-change->tx ctx value)))

(defn writable-entity? [entity-val]
  ; If the db/id was not pulled, we cannot write through to the entity
  (cljs.core/boolean (:db/id entity-val)))

(defn ^:export keyword [val props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/keyword-input* val on-change! props]))

(defn ^:export string [val props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/input* val on-change! props]))

(defn ^:export long [val props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        on-change! (r/partial entity-change! ctx)]
    [input/validated-input val on-change!
     #(let [v (js/parseInt % 10)] (if (integer? v) v nil))
     (fnil str "")
     #(or (= "" %) (integer? (js/parseInt % 10)))
     props]))

(defn ^:export boolean [val props ctx]
  ; wrapper div lets us style block while constraining checkbox clickable surface area inline
  [:div (let [props (adapt-props props)] (update props :class #(str % (if (:disabled props) " disabled"))))
   [contrib.ui/easy-checkbox "" val
    (r/partial entity-change! ctx (not val))
    (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]])

(defn ^:export tristate-boolean [val props ctx]
  (letfn [(adapter [e]
            (case (.-target.value e)
              "" nil
              "true" true
              "false" false))
          (change! [ctx v]
            (entity-change! ctx (adapter v)))]
    (let [option-props {:disabled (or (cljs.core/boolean (:read-only props))
                                      (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))))}]
      [:select (-> (dissoc props :label-fn)
                   (merge {:value (if (nil? val) "" (str val))
                           :on-change (r/partial change! ctx)}))
       [:option (assoc option-props :key true :value "true") "True"]
       [:option (assoc option-props :key false :value "false") "False"]
       [:option (assoc option-props :key :nil :value "") "--"]])))

(defn ^:export dbid [val props ctx]
  [input/id-input val
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn ^:export instant [val props ctx]
  [recom-date val
   (r/partial entity-change! ctx)
   (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])

(defn- code-mode [val props ctx]
  (let [props (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
        control (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                  :hyperfiddle.ui.layout/block contrib.ui/code
                  :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)]
    [control val (r/partial entity-change! ctx) props]))

(defn ^:export code [val props ctx]
  (let [props (-> props
                  (update :mode #(or % "clojure"))
                  (assoc :parinfer @(r/fmap :hyperfiddle.ide/parinfer (:hyperfiddle.ide/user ctx))))]
    [code-mode val props ctx]))

(defn ^:export css [val props ctx]
  [code-mode val (update props :mode #(or % "css")) ctx])

(defn ^:export markdown-editor [val props ctx]              ; This is legacy; :mode=markdown should be bound in userland
  (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/block contrib.ui/code
                 :hyperfiddle.ui.layout/table contrib.ui/code-inline-block)]
    ;code has backwards args - props first
    [widget val (r/partial entity-change! ctx)
     (-> props
         (update :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))
         (assoc :mode "markdown" :lineWrapping true))]))

(defn ^:export edn-many [val props ctx]
  (letfn [(change! [ctx value user-val]
            (let [user-val (set user-val)
                  rets (set/difference value user-val)
                  adds (set/difference user-val value)]
              (context/with-tx! ctx (tx/edit-entity @(r/fmap :db/id (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data]))
                                                    (:hypercrud.browser/attribute ctx)
                                                    rets adds))))]
    (let [valueType @(context/hydrate-attribute ctx (:hypercrud.browser/attribute ctx) :db/valueType :db/ident)
          value (set (if (= valueType :db.type/ref) (map :db/id val) val))
          widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                   :hyperfiddle.ui.layout/block contrib.ui/edn
                   :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block)]
      [widget value
       (r/partial change! ctx value)
       (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))])))

(defn ^:export edn [val props ctx]
  (let [widget (case (:hyperfiddle.ui/layout ctx :hyperfiddle.ui.layout/block)
                 :hyperfiddle.ui.layout/table contrib.ui/edn-inline-block
                 :hyperfiddle.ui.layout/block contrib.ui/edn)]
    [widget val
     (r/partial entity-change! ctx)
     (update props :read-only #(or % (not @(r/fmap writable-entity? (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])))))]))

;(defn textarea* [{:keys [value on-change] :as props}]       ; unused
;  (let [on-change #(let [newval (.. % -target -value)]
;                     (on-change [value] [newval]))]
;    [:textarea (assoc props :on-change on-change)]))
